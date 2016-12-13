# todo

## mvp
1. Fixup haddock docs and add to /docs directory for github.io. Use `stack build --haddock` to check coverage and get output
2. Parse `DOCKMASTER_HOME` from config.yml PATHS into `get_env "DOCKMASTER_HOME" ~> "~/.dockmaster/home" or whatever

## later
0. --local flag to execute on local machine instead of docker-machine targets
1. Use monad transformer to stack Sh on top of Either (error transformer)
2. Read up on exception handling link in haskell-fun-times resources
