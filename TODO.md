# todo

## mvp
1. Parse `~` and `$HOME` from config.yml PATHS into actual home directory (Sh FilePath)
2. Parse `DOCKMASTER_HOME` from config.yml PATHS into `get_env "DOCKMASTER_HOME" ~> "~/.dockmaster/home" or whatever
3. Implement hook firing

## later
1. Use monad transformer to stack Sh on top of Either (error transformer)
2. Read up on exception handling link in haskell-fun-times resources
