# lightemacs - WIP (Work in progress)

WIP (Work in progress)

## Features

- Reduced clutter: A `var/` directory (e.g., `~/.emacs.d/var/`) is used to store all files that Emacs normally places in the base directory (e.g., `~/.emacs.d`). By default, Emacs stores configuration files, caches, backups, and other data within `~/.emacs.d`, which can accumulate over time and complicate management.
- Ensure that all Elisp libraries undergo both byte-compilation and native-compilation via compile-angel.
