#
# Makefile reference vars :
#  https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables
#

#
# common targets
#
NAMESPACE:=dockmaster
prefix = $(DESTDIR)/usr/local/bin

.PHONY: all clean clean-% dockerbuild-% install uninstall

all: $(NAMESPACE)

clean:
	rm -rf $(CURDIR)/dist $(CURDIR)/.stack-work

clean-%:
	rm -rf $(CURDIR)/dist/$*

clean-tests: clean
	@rm -rf $(CURDIR)/tests/bats/tmp

clean-dockerbuilds:
	for id in $$(docker images -q makefile-$(NAMESPACE)-*) ; do docker rmi  $$id ; done

dockerbuild-%:
	echo "--- building Dockerfiles from $*/ ---"
	docker build \
	  --build-arg NAMESPACE=$(NAMESPACE) \
		--tag makefile-$(NAMESPACE)-$* \
		  $*/

install: dist/dm dist/dmc
	$(info * installing into $(prefix))
  # use mkdir vs. install -D/d (macos portability)
	@mkdir -p $(prefix)
	@cp -a dist/ $(prefix)/

uninstall:
	rm -rf  $(prefix)/dm $(prefix)/dmc

#
# app targets
#

RELEASE_TAG ?= $(shell git rev-parse --abbrev-ref HEAD)
RELEASE_SHA ?= $(shell git rev-parse --short HEAD)
BUILD_OPTS =

.PHONY: $(NAMESPACE) compile-%

$(NAMESPACE): compile-dm compile-dmc
compile-%: clean-%
	$(info * building dist/$* ...)
	@( \
		cd $(CURDIR) ; \
		mkdir -p dist ; \
		sed \
			-e 's|@VERSION@|$(RELEASE_TAG)|' \
			-e 's|@BUILD@|$(shell echo "$(RELEASE_SHA)" | cut -c1-7)|' \
			--in-place=.bak \
			src/Options/Utils.hs ; \
		PATH="$(CURDIR)/dist:$$PATH" ; \
	  stack install --system-ghc --local-bin-path dist $(BUILD_OPTS) dockmaster:exe:$* ; \
	  mv src/Options/Utils.hs.bak src/Options/Utils.hs \
	)

#
# test targets
#

TEST ?=
SKIP_NETWORK_TEST ?=
DOCKER_SOCKET ?= /var/run/docker.sock
DOCKER_GROUP_ID ?= $(shell ls -ln $(DOCKER_SOCKET) | awk '{print $$4}')
# for docker-for-mac, we also add group-id of 50 ("authedusers") as moby distro seems to auto bind-mount /var/run/docker.sock w/ this ownership
DOCKER_FOR_MAC_WORKAROUND := $(shell [[ "$$OSTYPE" == darwin* || "$$OSTYPE" == macos* ]] && echo "--group-add=50")

.PHONY: tests

tests: dockerbuild-tests clean-tests
	docker run -it --rm -u $$(id -u):$$(id -g) $(DOCKER_FOR_MAC_WORKAROUND) \
		--group-add=$(DOCKER_GROUP_ID) \
		--device=/dev/tty0 --device=/dev/console \
		-v $(CURDIR):/$(CURDIR) \
		-v $(DOCKER_SOCKET):/var/run/docker.sock \
		-e SKIP_NETWORK_TEST=$(SKIP_NETWORK_TEST) \
		--workdir $(CURDIR) \
		  makefile-$(NAMESPACE)-tests bats tests/bats/$(TEST)

#
# release targets
#

.PHONY: release prerelease _mkrelease

RELEASE_VERSION ?=
RELEASE_BRANCH ?=
MERGE_BRANCH ?= HEAD

GH_TOKEN ?=
GH_URL ?= https://api.github.com
GH_UPLOAD_URL ?= https://uploads.github.com
GH_PROJECT:=dockerland/dockmaster

REMOTE_GH:=origin
REMOTE_LOCAL:=local

prerelease: PRERELEASE = true
prerelease: BUILD_OPTS = --ghc-options '-fPIC -optl-static -optl-pthread'
prerelease: _mkrelease

release: PRERELEASE = false
release: BUILD_OPTS = --ghc-options '-fPIC -optl-static -optl-pthread'
release: _mkrelease

_mkrelease: RELEASE_TAG = v$(RELEASE_VERSION)$(shell $(PRERELEASE) && echo '-pr')
_mkrelease: _release_check $(NAMESPACE)
	git push --force $(REMOTE_LOCAL) $(MERGE_BRANCH):$(RELEASE_BRANCH)
	git push $(REMOTE_GH) $(RELEASE_BRANCH)
	$(eval RELEASE_SHA=$(shell git rev-parse $(RELEASE_BRANCH)))
	$(eval CREATE_JSON=$(shell printf '{"tag_name": "%s","target_commitish": "%s","draft": false,"prerelease": %s}' $(RELEASE_TAG) $(RELEASE_SHA) $(PRERELEASE)))
	( \
	  cd $(CURDIR) ; \
		id=$$(curl -sLH "Authorization: token $(GH_TOKEN)" $(GH_URL)/repos/$(GH_PROJECT)/releases/tags/$(RELEASE_TAG) | jq -Me .id) ; \
		if [ $$id = "null" ]; then \
		  echo "  * attempting to create release $(RELEASE_TAG) ..." ; \
      id=$$(curl -sLH "Authorization: token $(GH_TOKEN)" -X POST --data '$(CREATE_JSON)' $(GH_URL)/repos/$(GH_PROJECT)/releases | jq -Me .id) ; \
		else \
		  echo "  * attempting to update release $(RELEASE_TAG) ..." ; \
			git push $(REMOTE_GH) :$(RELEASE_TAG) ; \
		  curl -sLH "Authorization: token $(GH_TOKEN)" -X PATCH --data '$(CREATE_JSON)' $(GH_URL)/repos/$(GH_PROJECT)/releases/$$id ; \
		fi ; \
		[ $$id = "null" ] && echo "  !! unable to create release" && exit 1 ; \
		echo "  * uploading dist/dm to release $(RELEASE_TAG) ($$id) ..." ; \
    curl -sL -H "Authorization: token $(GH_TOKEN)" -H "Content-Type: text/x-shellscript" --data-binary @"dist/dm" -X POST $(GH_UPLOAD_URL)/repos/$(GH_PROJECT)/releases/$$id/assets?name=dm &>/dev/null ; \
		echo "  * uploading dist/dmc to release $(RELEASE_TAG) ($$id) ..." ; \
    curl -sL -H "Authorization: token $(GH_TOKEN)" -H "Content-Type: text/x-shellscript" --data-binary @"dist/dmc" -X POST $(GH_UPLOAD_URL)/repos/$(GH_PROJECT)/releases/$$id/assets?name=dmc &>/dev/null ; \
	)

	$(info * publishing to get.iceburg.net/$(NAMESPACE)/latest-$(RELEASE_BRANCH)/)
	@( \
                set -e ; \
          cd $(CURDIR)/dist ; \
                for file in * ; do \
                  echo "# @$(NAMESPACE)_UPDATE_URL=http://get.iceburg.net/$(NAMESPACE)/latest-$(RELEASE_BRANCH)/$$file" >> $$file ; \
                done ; \
                drclone sync . iceburg_s3:get.iceburg.net/$(NAMESPACE)/latest-$(RELEASE_BRANCH) ; \
        )


#
# sanity checks
.PHONY: _release_check _gh_check _wc_check

SKIP_WC_CHECK ?=

_release_check: _wc_check _git_check _gh_check
	@test ! -z "$(RELEASE_VERSION)" || ( \
	  echo "  * please provide RELEASE_VERSION - e.g. '1.0.0'" ; \
		echo "     'v' and '-pre' are automatically added" ; \
		false )

_git_check:
	$(info ensure release branches, local remote)
	@test ! -z "$(RELEASE_BRANCH)" || ( \
		echo "  * please provide RELEASE_BRANCH - e.g. 'v1' 'v2' 'release' 'prerelease'" ; \
		false )
	@git rev-parse --verify "$(RELEASE_BRANCH)" &>/dev/null || { \
    read -r -p "Release Branch $(RELEASE_BRANCH) does not exist. Create? [y/N] " CONTINUE; \
    [[ "$$CONTINUE" =~ [Yy] ]] || { exit 1 ; } ; \
		git branch --track $(RELEASE_BRANCH) $(MERGE_BRANCH) ; \
	}
	@git ls-remote --exit-code --heads $(REMOTE_LOCAL) &>/dev/null || \
	  git remote add $(REMOTE_LOCAL) $(shell git rev-parse --show-toplevel)
	@git checkout $(MERGE_BRANCH)

_gh_check:
	$(info checking communication with GitHub...)
	@type jq&>/dev/null || ( \
	  echo "  * jq (https://github.com/stedolan/jq) missing from your PATH." ; \
		false )
	@test ! -z "$(GH_TOKEN)" || ( \
	  echo "  * please provide GH_TOKEN - your GitHub personal access token" ; \
		false )
	$(eval CODE=$(shell curl -iso /dev/null -w "%{http_code}" -H "Authorization: token $(GH_TOKEN)" $(GH_URL)))
	@test "$(CODE)" = "200" || ( \
	  echo "  * request to GitHub failed to return 200 ($(CODE)). " ; \
		false )

ifdef SKIP_WC_CHECK
  _wc_check:
	  $(info skipping working copy check...)
else
  _wc_check:
		$(info checking for clean working copy...)
		@test -z "$$(git status -uno --porcelain)" || ( \
			echo "   * please stash or commit changes before continuing" ; \
			echo "     or set SKIP_WC_CHECK=true" ; false )
endif
