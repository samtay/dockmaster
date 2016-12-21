#
# Makefile reference vars :
#  https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html#Automatic-Variables
#

#
# common targets
#
NAMESPACE:=dm
DMC:=dmc
prefix = $(DESTDIR)/usr/local/bin

.PHONY: all clean
all: $(NAMESPACE) # dmc not yet completed, not built by default

clean:
	rm -rf $(CURDIR)/dist

#
# app targets
#
.PHONY: $(NAMESPACE) $(DMC) tests
$(NAMESPACE):
	$(call compile,$(NAMESPACE))

$(DMC):
	$(call compile,$(DMC))

tests:
	@( \
	  cd $(CURDIR) ; \
	  stack test \
	)

define compile
	$(info * building dist/$(1)) \
	@( \
	  cd $(CURDIR) ; \
	  rm -rf dist/$(1) ; \
	  mkdir -p dist ; \
	  stack build dockmaster:exe:$(1) --copy-bins --local-bin-path dist \
	)
endef
	
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
prerelease: _mkrelease

release: PRERELEASE = false
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
		echo "  * uploading dist/$(NAMESPACE) to release $(RELEASE_TAG) ($$id) ..." ; \
    curl -sL -H "Authorization: token $(GH_TOKEN)" -H "Content-Type: text/x-shellscript" --data-binary @"dist/$(NAMESPACE)" -X POST $(GH_UPLOAD_URL)/repos/$(GH_PROJECT)/releases/$$id/assets?name=$(NAMESPACE) &>/dev/null ; \
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
