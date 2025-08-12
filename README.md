# lightemacs - WIP (Work in progress)

WIP (Work in progress)

## Default modules

### Vim keybindings

By default, **evil-mode** is enabled. (Users who prefer not to use Vim keybindings can easily disable it.)

Interesting default parameters:

- The default leader key is: `,`

The following packages are enabled alongside **evil-mode**:

- **mod-evil-toggle-comment**: Comment or uncomment text in Normal or Visual mode by pressing `gc`.

- **mod-evil-snipe**: Provides two-character motions for rapid navigation within text, similar to Evil’s built-in `f`/`F`/`t`/`T` commands, but with incremental highlighting of potential targets as you type. By default, `s` (forward) and `S` (backward) are bound to `evil-snipe-s` and `evil-snipe-S`, respectively. **Usage:** Pressing `s` in normal mode prompts you to type two characters, then jumps the cursor to the nearest matching occurrence while highlighting all matches incrementally.

- **mod-evil-surround**: Enables text surrounding in visual state using `S<textobject>` or `gS<textobject>`. For example, selecting text and pressing `S"` will wrap it in double quotes.

### mod-keybindings

Defines the following key bindings:
- Increase or decrease the text scale using Ctrl combined with `+` or `-`.

### mod-savehist

The **mod-savehist** module configures **savehist**, a built-in Emacs feature that preserves the minibuffer history between sessions. It saves the history of inputs in the minibuffer, such as commands, search strings, and other prompts, to a file. This allows users to retain their minibuffer history across Emacs restarts.

### mod-saveplace

The **mod-saveplace** module enables `save-place-mode`, which makes Emacs remember the last location within a file when reopened. This facilitates resuming work exactly where it was left off.

### Other modules enabled by default

- **compile-angel**: Compile-angel speeds up Emacs by ensuring that all Elisp libraries are both byte-compiled and native-compiled.
- **vim-tab-bar**: Enhances Emacs’ built-in tab bar with a minimalist, Vim-inspired design that automatically adapts to the current Emacs theme.

## Features

- Reduced clutter: A `var/` directory (e.g., `~/.emacs.d/var/`) is used to store all files that Emacs normally places in the base directory (e.g., `~/.emacs.d`). By default, Emacs stores configuration files, caches, backups, and other data within `~/.emacs.d`, which can accumulate over time and complicate management.
- Ensure that all Elisp libraries undergo both byte-compilation and native-compilation via the compile-angel package.
- Enable Vim keybindings through Evil mode, including evil-surround, evil-snipe, and vim-tab-bar. These features can be easily disabled for users who prefer not to use Vim keybindings.
