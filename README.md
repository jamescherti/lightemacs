# lightemacs - WIP (Work in progress)

Fast and lightweight Emacs framework.

## Default modules

### Better minibuffer and navigation (mod-consult, mod-embark, and mod-vertico)

[Vertico](https://github.com/minad/vertico), [Consult](https://github.com/minad/consult), and [Embark](https://github.com/oantolin/embark) collectively enhance Emacs' completion and navigation capabilities.

Vertico provides a vertical completion interface, making it easier to navigate and select from completion candidates (e.g., when `M-x` is pressed).

Consult offers a suite of commands for efficient searching, previewing, and interacting with buffers, file contents, and more, improving various tasks.

Embark integrates with these tools to provide context-sensitive actions and quick access to commands based on the current selection, further improving user efficiency and workflow within Emacs. Together, they create a cohesive and powerful environment for managing completions and interactions.

### Better Syntax highlighting ( mod-treesit-auto )

The **mod-treesit-auto** module automatically installs and enables Tree-sitter major modes in Emacs 29 and later. If the Tree-sitter parser is unavailable or incompatible, it falls back to the original major mode.

Tree-sitter is an incremental parsing system introduced in Emacs 29 that delivers precise, high-performance syntax highlighting. It supports a wide range of programming languages, including Bash, C, C++, C#, CMake, CSS, Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML, Elisp, Lua, and many others.

### Vim keybindings (mod-evil, mod-evil-snipe, mod-evil-surround, and evil-toggle-comment)

By default, **evil-mode** is enabled. (Users who prefer not to use Vim keybindings can easily disable it.)

Interesting default parameters:

- The default leader key is: `,`

The following packages are enabled alongside **evil-mode**:

- **mod-evil-toggle-comment**: Comment or uncomment text in Normal or Visual mode by pressing `gc`.

- **mod-evil-snipe**: Provides two-character motions for rapid navigation within text, similar to Evil’s built-in `f`/`F`/`t`/`T` commands, but with incremental highlighting of potential targets as you type. By default, `s` (forward) and `S` (backward) are bound to `evil-snipe-s` and `evil-snipe-S`, respectively. **Usage:** Pressing `s` in normal mode prompts you to type two characters, then jumps the cursor to the nearest matching occurrence while highlighting all matches incrementally.

- **mod-evil-surround**: Enables text surrounding in visual state using `S<textobject>` or `gS<textobject>`. For example, selecting text and pressing `S"` will wrap it in double quotes.

### Keybindings (mod-keybindings)

Defines the following key bindings:
- Increase or decrease the text scale using Ctrl combined with `+` or `-`.

### Save history (mod-savehist)

The **mod-savehist** module configures **savehist**, a built-in Emacs feature that preserves the minibuffer history between sessions. It saves the history of inputs in the minibuffer, such as commands, search strings, and other prompts, to a file. This allows users to retain their minibuffer history across Emacs restarts.

### Save and restore cursor (mod-saveplace)

The **mod-saveplace** module enables `save-place-mode`, which makes Emacs remember the last location within a file when reopened. This facilitates resuming work exactly where it was left off.

### Auto revert buffer to reflect changes made to the underlying file on disk (mod-autorevert)

Auto-revert is a feature that automatically updates the contents of a buffer to reflect changes made to the underlying file on disk.

### Recent files (mod-recentf)

Recentf maintains a list of recently accessed files, making it easier to reopen files you have worked on recently.

In addition to its built-in capabilities, the **mod-recentf** module provides the following enhancements:
- Inserts the current file at the beginning of the recent files list upon buffer switch.
- Cleans up the recent files list when quitting Emacs, prior to its automatic saving.
- Decrease recentf-mode verbosity by restricting its messages to the `*Messages*` buffer, preventing display in the minibuffer

### Other modules enabled by default

- **compile-angel**: Compile-angel speeds up Emacs by ensuring that all Elisp libraries are both byte-compiled and native-compiled.
- **vim-tab-bar**: Enhances Emacs’ built-in tab bar with a minimalist, Vim-inspired design that automatically adapts to the current Emacs theme.

## Features

- Reduced clutter: A `var/` directory (e.g., `~/.emacs.d/var/`) is used to store all files that Emacs normally places in the base directory (e.g., `~/.emacs.d`). By default, Emacs stores configuration files, caches, backups, and other data within `~/.emacs.d`, which can accumulate over time and complicate management.
- Ensure that all Elisp libraries undergo both byte-compilation and native-compilation via the compile-angel package.
- Enable Vim keybindings through Evil mode, including evil-surround, evil-snipe, and vim-tab-bar. These features can be easily disabled for users who prefer not to use Vim keybindings.
