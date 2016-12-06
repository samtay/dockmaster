# dockmaster [![Build Status](https://travis-ci.org/SamTay/dockmaster.svg?branch=master)](https://travis-ci.org/SamTay/dockmaster)
yaml loving docker-compose orchestration

### why?
Dockmaster was built for convenience. Some of these conveniences are:

- **machine management**: with multiple machines and compositions, dockmaster allows us to keep the machine targets for each composition stored in yaml configuration

 ```yaml
 targets:
   - name: node-a
     type: docker-machine
 
     # optional, defaults to name
     machine: node-a
 ```
- **hooks**: automate pre & post hooks around arbitrary `docker-compose` commands

 ```yaml
 commands:
   - up:
       pre_hooks:
         - file: relative_path/to/hook.sh
         - file: /absolute/path/to/hook.sh
         - shell: rm -rf .working
   - wiggle:
       compose: false
       # ^^^ does not call docker-compose between pre- and post- hooks.
       pre_hooks:
         - file: wiggle.sh
 ```
- **templating**: template your docker-compose files to reuse common patterns

 ```yaml
 file:
   path: docker-compose.j2
   config:
     - /etc/dockmaster/defaults.yml
     - env.vars
 ```

### usage
Dockmaster provides binaries `dm` and `dmc` which are for interacting with `dockmaster.yml` files and setting config values, respectively:
```shell
# dm [-c PATH] COMMAND [ARGS]
# -c specifies composition directory, will default to "." if not specified
#
# dmc OPTION [VALUE]
# if value supplied, it is set, otherwise it is retrieved

# executes `docker-compose down` against `./dockmaster.yml`
$ dm down

# executes `docker-compose up -d` on subdirectory composition
$ dm -c ./composition -- up -d

# adds composition listing directory to configuration
# this makes it easier to issue commands to compositions from anywhere
$ dmc DOCKMASTER_COMPOSITION_DIRS $HOME/git/docker-repo/compositions

# executes `docker-compose logs` on $HOME/git/docker-repo/compositions/deploybot
$ dm -c deploybot logs
```

### installation
Dockmaster is still under development. Once complete, installation details will be provided.
