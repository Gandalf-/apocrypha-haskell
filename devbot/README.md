# Devbot

Devbot is a command orchestration tool, similar to `cron`. Actions are
sequences of commands, potentially requiring a condition to to run, that are
run at intervals and checked for errors.

There are three executables:
- devbot
- devbot_list
- devbot_status

# Devbot

Reads the configuration in the local Apocrypha database, builds a schedule of
events and starts executing them. The event data must already have been loaded
into the database.

## Example configuration
```
events:

  dotfiles-fetch:
    action:
      - cd ~/DotFiles
      - git fetch
    interval: daily
    require: network

requirements:
  network: nc -w 1 -z 8.8.8.8 53
```

Valid intervals:
- weekly
- daily
- hourly
- <number> of seconds

# Devbot List

Shows the current state of all actions, how often they run, any errors, any
requirements, what commands are run, and the next time to run.

```
$ devbot_list

dotfiles-fetch
    cd ~/DotFiles
    git fetch
    every 24 hours, took 1 seconds, requires network, next in 19 hours

```

# Devbot Status

Shows the current state of the devbot daemon as a single character, useful for
embedding in your Tmux status line.

- Running  ✓
- Stopped  ✗
- StalePid ?
- Database !
