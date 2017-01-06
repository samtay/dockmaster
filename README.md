# dockmaster [![Build Status](https://travis-ci.org/dockerland/dockmaster.svg?branch=master)](https://travis-ci.org/dockerland/dockmaster)
yaml loving docker-compose orchestration

## why?
Dockmaster was built for convenience. Some of these conveniences are:

- **machine management**: with multiple machines and compositions, dockmaster allows us to keep the machine targets for each composition stored in yaml configuration:
 ```yaml
 targets:
   - name: node-a
     type: docker-machine
 
     # optional, defaults to name
     machine: node-a
 ```
 With the configuration above, `dm` will connect to docker-machine `node-a` when executing any command.

- **hooks**: automate pre & post hooks around arbitrary `docker-compose` commands:
 ```yaml
 commands:
   up:
     pre_hooks:
       - file: relative_path/to/hook.sh
       - file: /absolute/path/to/hook.sh
       - shell: rm -rf .working
   wiggle:
     run_compose: false
     # ^^^ does not call docker-compose between pre- and post- hooks.
     pre_hooks:
       - file: wiggle.sh
 ```

- **templating**: avoid carpal tunnel. Template your docker-compose files to reuse common patterns:
 ```yaml
 compose:
   file:
     - path: docker-compose.yml
     - path: docker-compose.j2
       type: template
       config:
         - some.vars
         - many.env
 ```
 The `files` node will compile `docker-compose.j2` with variables from the `config` files.
 This ends up calling `docker-compose` with successive file arguments, e.g.
 ```bash
 docker-compose -f docker-compose.yml -f docker-compose.j2.yml
 ```

- **global flags**: automate environment variables to be present during command execution:
 ```yaml
 env:
   files:
     - "somefile.env"
     - "somefile.json"
     - "somefile.xml"
     - "somefile.yml"
   vars:
     ROBS_FAVORITE_NUM: "64"
 ```
 and flags that should always be passed to `docker-compose`:
 ```yaml
 compose:
   flags:
     - "--project-name aaa"
 ```

## run dockmaster
### dependencies
Dockmaster requires the following tools installed on your system at run time:

0. [docker-compose](https://docs.docker.com/compose/install/)
1. [docker-machine](https://docs.docker.com/machine/install-machine)
2. [cop](https://github.com/jasmith590/COP)

Alternatively, check out [dex](https://github.com/dockerland/dex) which will allow you to get all of these tools via dex images (docker executables). Dex only requires [docker](https://www.docker.com/) and [git](https://git-scm.com/) and can run pretty much anywhere.

### installation

#### from a release
Download a [release](https://github.com/dockerland/dockmaster/releases) and copy it to a folder in your `$PATH`:
```bash
curl -L "https://raw.githubusercontent.com/dockerland/dockmaster/master/install.sh" | sudo sh
```

#### from source
```bash
git clone git@github.com:dockerland/dockmaster.git
cd dockmaster
make
sudo make install
```

## usage
Dockmaster provides binaries `dm` and `dmc` which are for interacting with `dockmaster.yml` files and setting config values, respectively.
#### reference
```
$ dm --help
dm - yaml loving docker compose orchestration

Usage: dm [-c|--composition PATH] [-v|--verbose] [-l|--local] COMMAND [ARGS]
  Orchestrate your docker-compose

Available options:
  -h,--help                Show this help text
  -c,--composition PATH    Composition directory. Note this can be relative to
                           any directories specified in global
                           config. (default: ".")
  -v,--verbose             Verbose output.
  -l,--local               Execute without connecting to configured docker
                           machine.
  COMMAND                  Command to forward to docker-compose.
  ARGS                     Any arguments/options to forward to docker-compose
                           COMMAND.

$ dmc --help
dmc - dockmaster configuration modifiers

Usage: dmc COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  set                      Set value
  get                      Get value
  unshift                  Unshift value (for arrays)
  push                     Push value (for arrays)
  shift                    Shift value (for arrays)
  pop                      Pop value (for arrays)
  cat                      Cat full configuration
  ls                       List available config fields
```

#### `dm --composition` resolution
```shell
# executes `docker-compose down` against `dockmaster.yml` in CWD
$ dm down

# executes `docker-compose up -d` on `./composition`
$ dm -c composition -- up -d

# executes `docker-compose up -d` on `/absolute/composition`
$ dm -c /absolute/composition -- up -d

# adds composition listing directory to configuration
# this makes it easier to issue commands to compositions from anywhere
$ dmc push PATHS $HOME/git/docker-repo/compositions
Saved successfully
The new value for PATHS is:
- $HOME/git/docker-repo/compositions

# executes `docker-compose logs` on $HOME/git/docker-repo/compositions/deploybot
$ dm -c deploybot logs
```

#### `dm --verbose` information
Use verbose output for more information. This will provide a playback of all commands issued by `dm`, and also help troubleshoot composition directory resolution:
```shell
$ dm -v -c nonexistent up
Looking in directory nonexistent
Looking in directory /home/user/path2/nonexistent
Looking in directory /home/user/path3/nonexistent
Looking in directory /home/user/otherpath/nonexistent
Could not resolve dockmaster working directory.
```

#### `dm --local` to skip machine targets
The local flag disregards connecting to any specified docker machines, and just runs `docker-compose` directly. If you are not connected to a docker machine, this will run "locally" on your machine; otherwise, it will run on the currently configured docker machine. (Use `docker-machine active` to test if you are connected to an external machine or not.)
```shell
# get log output from node-b
$ dm -c runsOnNodeB logs

# get log output from local
$ dm -l -c runsOnNodeB logs
```

#### configuration management
The configuration file for dockmaster is found at runtime via the following files in order of precedence:

1. `$DOCKMASTER_CONFIG`
2. `$DOCKMASTER_HOME/config.yml`, where `$DOCKMASTER_HOME` defaults to user's `~/.dockmaster`
3. `/etc/dockmaster/config.yml`

`dmc` will never modify the global config and always prefers `$DOCKMASTER_HOME/config.yml`.
If the file is not present, it will either be created with an empty configuration, or copy the current resolved configuration file, e.g. a possibly distributed `/etc/dockmaster/config.yml`.

The `dmc --help` output is largely straightforward, but here are some examples anyway:
```shell
# dmc ls will show you available config fields
$ dmc ls
Array config fields:
PATHS

$ dmc set PATHS path1 path2 path3 path4 path5
Saved successfully
The new value for PATHS is:
- path1
- path2
- path3
- path4
- path5

$ dmc pop PATHS
Saved successfully
The new value for PATHS is:
- path1
- path2
- path3
- path4

$ dmc shift PATHS 2
Saved successfully
The new value for PATHS is:
- path3
- path4

$ dmc unshift PATHS path0 path1 path2
Saved successfully
The new value for PATHS is:
- path0
- path1
- path2
- path3
- path4

$ dmc push PATHS path5 path6
Saved successfully
The new value for PATHS is:
- path0
- path1
- path2
- path3
- path4
- path5
- path6

# dmc cat will show full configuration
$ dmc cat
PATHS:
- path0
- path1
- path2
- path3
- path4
- path5
- path6
```
