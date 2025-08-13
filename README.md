# Lightemacs - Fast and Lightweight Emacs framework

The Lightemacs project is a fast, lightweight Emacs framework that uses modern Emacs features and modern packages.

All modules and packages are activated only when required, allowing Lightemacs to start very quickly.

Here are the features that are enabled by default:
- Ensure that all Elisp libraries are both byte-compiled and native-compiled to speed up Emacs.
- Vim keybindings (Evil) with additional functionality, including commenting/uncommenting, two-character search using the `s` key (as an alternative to the `f` key), and surrounding text in visual state. (Vim keybindings can be disabled.)
- Ensure that all adjustments made with `text-scale-increase` and `text-scale-decrease` are persisted and restored across sessions
- Improved undo/redo functionality with persistent undo history saved and restored across Emacs sessions, even after restarts.
- Preserve minibuffer history between sessions (savehist), persist and restore cursor position (saveplace), automatically update buffer contents to reflect changes in the underlying file on disk (autorevert), and maintain a list of recently accessed files (recentf).
- The Yasnippet template system that enhances text editing by enabling users to define and use snippets.
- Better Syntax highlighting with Tree-sitter. (If the Tree-sitter parser is unavailable or incompatible, it falls back to the original major mode.)
- Additional filetypes: markdown-mode.
- Automatically removes trailing whitespace and blank lines at the end of the buffer when saving
- Emacs Lisp editing: Maintain consistent indentation of Elisp code during editing.
- Dired: Configure dired to group directories first and enable dired-filter to hide dotfiles, omit specified files, and exclude files listed in `.gitignore`.
- Change the default Ellipsis using the `lightemacs-ellipsis` variable, which defaults to `" ▼"` String used to indicate folded sections in `org-mode`, `outline-mode`, `outline-minor-mode`...
- Save and restore the default theme using the `lightemacs-default-theme` variable.
- And more.

![](https://www.jamescherti.com/misc/screenshot-minimal-emacs-3.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [Lightemacs - Fast and Lightweight Emacs framework](#lightemacs---fast-and-lightweight-emacs-framework)
  - [Requirements](#requirements)
  - [Install Lightemacs](#install-lightemacs)
    - [Install Lightemacs into ~/.emacs.d](#install-lightemacs-into-emacsd)
    - [Alternative: Install Lightemacs into ~/.lightemacs.d](#alternative-install-lightemacs-into-lightemacsd)
  - [Update Lightemacs](#update-lightemacs)
  - [Customizations](#customizations)
    - [Never modify init.el and early-init.el. Modify these instead...](#never-modify-initel-and-early-initel-modify-these-instead)
    - [How to enable the menu-bar, the tool-bar, dialogs, the contextual menu, and tooltips?](#how-to-enable-the-menu-bar-the-tool-bar-dialogs-the-contextual-menu-and-tooltips)
  - [Features and modules enabled by default](#features-and-modules-enabled-by-default)
    - [Default theme (mod-default-theme)](#default-theme-mod-default-theme)
    - [Better minibuffer and navigation (mod-consult, mod-embark, and mod-vertico, mod-marginalia)](#better-minibuffer-and-navigation-mod-consult-mod-embark-and-mod-vertico-mod-marginalia)
    - [Better File Manager (mod-dired and mod-dired-filter)](#better-file-manager-mod-dired-and-mod-dired-filter)
    - [Better completion (mod-corfu and mod-cape)](#better-completion-mod-corfu-and-mod-cape)
    - [Better Syntax highlighting (mod-treesit-auto)](#better-syntax-highlighting-mod-treesit-auto)
    - [Efficient template expansion with snippets (mod-yasnippet and mod-yasnippet-snippets)](#efficient-template-expansion-with-snippets-mod-yasnippet-and-mod-yasnippet-snippets)
    - [Better undo/redo (mod-undo-fu and undo-fu-session)](#better-undoredo-mod-undo-fu-and-undo-fu-session)
    - [Vim Keybindings (mod-evil, mod-evil-snipe, mod-evil-surround, and evil-toggle-comment)](#vim-keybindings-mod-evil-mod-evil-snipe-mod-evil-surround-and-evil-toggle-comment)
    - [Keybindings (mod-keybindings)](#keybindings-mod-keybindings)
    - [Code folding based on indentation (mod-outline-indent)](#code-folding-based-on-indentation-mod-outline-indent)
    - [Save History (mod-savehist)](#save-history-mod-savehist)
    - [Save and Restore Cursor (mod-saveplace)](#save-and-restore-cursor-mod-saveplace)
    - [Auto Revert Buffer to Reflect Changes Made to the Underlying File on Disk (mod-autorevert)](#auto-revert-buffer-to-reflect-changes-made-to-the-underlying-file-on-disk-mod-autorevert)
    - [Persist and Restore Text Scale (mod-persist-text-scale)](#persist-and-restore-text-scale-mod-persist-text-scale)
    - [Automatically Remove Trailing Whitespace before Saving a Prog-mode Buffer](#automatically-remove-trailing-whitespace-before-saving-a-prog-mode-buffer)
    - [Recent files (mod-recentf)](#recent-files-mod-recentf)
    - [Other Modules Enabled by Default](#other-modules-enabled-by-default)
    - [Other Features](#other-features)
    - [Useful variables](#useful-variables)
      - [Ellipsis](#ellipsis)
  - [Author and license](#author-and-license)
  - [Links](#links)

<!-- markdown-toc end -->

## Requirements

- Emacs >= 29.1
- Git

## Install Lightemacs

- **Important:** Ensure that the `~/.emacs` and `~/.emacs.el` files do not exist. These files cause Emacs to ignore `~/.emacs.d/init.el`. This behavior is due to the way Emacs searches for initialization files ([more information](https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html#Find-Init)). **Simply delete the *~/.emacs* and *~/.emacs.el* files avoid this issue.**
- **Debug:** If a package or any other functionality is not working as expected, start Emacs with `emacs --debug-init` to enable debug mode and obtain the backtrace.
- **Prerequisite:** git

### Install Lightemacs into ~/.emacs.d

Execute the following command install this repository into `~/.emacs.d`:
```
git clone --recursive https://github.com/jamescherti/lightemacs ~/.emacs.d
```

### Alternative: Install Lightemacs into ~/.lightemacs.d

To install *Lightemacs* in a non-default directory, use the `--init-directory` Emacs option to specify your desired configuration path. For example, to install *Lightemacs* in `~/.lightemacs.d/`, follow these steps:

1. Clone the repository into `~/.lightemacs.d/` using:
   ```
   git clone https://github.com/jamescherti/lightemacs ~/.lightemacs.d
   ```

2. Start Emacs with the new configuration directory:
   ```
   emacs --init-directory ~/.lightemacs.d/
   ```

## Update Lightemacs

To update your Lightemacs configuration and its submodules, run the following commands:
```
git -C ~/.emacs.d pull --recurse-submodules
git -C ~/.emacs.d submodule update --init --recursive
```

## Customizations

### Never modify init.el and early-init.el. Modify these instead...

**The `init.el` and `early-init.el` files should never be modified directly** because they are intended to be managed by Git during an update.

The Lightemacs project is based on the [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) initialization files, which means it can be configured in exactly the same way as minimal-emacs.d.

These files allow you to further customize the initialization sequence:

- `~/.emacs.d/pre-init.el`: This file is loaded before `init.el`. Use it to set up variables or configurations that need to be available early in the initialization process but after `early-init.el`.

- `~/.emacs.d/post-init.el`: This file is loaded after `init.el`. It is useful for additional configurations or package setups that depend on the configurations in `init.el`.

- `~/.emacs.d/pre-early-init.el`: This file is loaded before `early-init.el`. Use it for configurations that need to be set even earlier in the startup sequence, typically affecting the initial setup of the Emacs environment.

- `~/.emacs.d/post-early-init.el`: This file is loaded after `early-init.el` but before `init.el`. It is useful for setting up configurations that depend on the early initialization but need to be set before the main initialization begins.

Always begin your `pre-init.el`, `post-init.el`, `post-early-init.el`, and `pre-early-init.el` files with the following header to prevent them from being byte-compiled and to activate lexical binding:
```elisp
;;; FILENAME.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
```

Replace `FILENAME.el` with the actual name and DESCRIPTION with a brief description of its purpose.

*(Only if you know what you're doing: Removing `no-byte-compile: t;` from your init files allows Emacs to compile them, improving load and execution speed. However, if you do so, you may need to add required dependencies. For example, if you're using `use-package`, add `(require 'use-package)` at the top of `post-init.el` to ensure all necessary `use-package` variables and functions are loaded.)*

**Important:** The examples in this README reference pre/post init files in the `~/.emacs.d/` directory, but the files `pre-early-init.el`, `post-early-init.el`, `pre-init.el`, and `post-init.el` should be placed in the same directory as `init.el` and `early-init.el`, regardless of their location.

### How to enable the menu-bar, the tool-bar, dialogs, the contextual menu, and tooltips?

The Lightemacs project is based on the [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) initialization files, which means it can be configured in exactly the same way as minimal-emacs.d.

**Note:** Enabling the tool-bar or menu-bar may slightly increase your startup time.

To customize your Emacs setup to include various user interface elements, you can use the following settings in your ``~/.emacs.d/pre-early-init.el``:

``` emacs-lisp
(setq minimal-emacs-ui-features '(context-menu tool-bar menu-bar dialogs tooltips))
```

These settings control the visibility of dialogs, context menus, toolbars, menu bars, and tooltips.

## Features and modules enabled by default

### Default theme (mod-default-theme)

The `mod-default-theme` loads the default theme. It can be configured via the `lightemacs-default-theme` variable, which defaults to `"tomorrow-night-deepblue"`. To customize this theme, modify the variable in your `~/.emacs/post-init.el` as follows:

```emacs-lisp
(setq lightemacs-default-theme 'tomorrow-night-deepblue)
```

The default theme, Tomorrow Night Deepblue Emacs Theme, is a a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes:
![](https://www.jamescherti.com/misc/screenshot-minimal-emacs-3.png)

### Better minibuffer and navigation (mod-consult, mod-embark, and mod-vertico, mod-marginalia)

Vertico, Consult, Marginalia, and Embark collectively enhance Emacs' completion and navigation capabilities:
- [Vertico](https://github.com/minad/vertico) provides a vertical completion interface, making it easier to navigate and select from completion candidates (e.g., when `M-x` is pressed).
- [Consult](https://github.com/minad/consult) offers a suite of commands for efficient searching, previewing, and interacting with buffers, file contents, and more, improving various tasks. *(Try `M-x consult-rg` to search your project using ripgrep, or `M-x consult-fd` to quickly locate files in your workspace.)*
- [Embark](https://github.com/oantolin/embark) integrates with these tools to provide context-sensitive actions and quick access to commands based on the current selection, further improving user efficiency and workflow within Emacs. Together, they create a cohesive and powerful environment for managing completions and interactions. *(While searching with `M-x consult-rg` or `M-x consult-fd`, you can use `M-x embark-export` to act on the search results. This allows you to export the matches to another buffer, perform batch operations, or open multiple files at once, providing an excellent way to manipulate and navigate large sets of results.)*
- [Marginalia](https://github.com/minad/marginalia) enriches minibuffer completions with contextual annotations. It Enhances Vertico by adding rich annotations to completion candidates, such as file sizes, documentation, or metadata.

![](https://github.com/minad/consult/blob/screenshots/consult-grep.gif?raw=true)

### Better File Manager (mod-dired and mod-dired-filter)

Configure dired to group directories first and enable dired-filter to hide dotfiles, omit specified files, and exclude files listed in `.gitignore`.

### Better completion (mod-corfu and mod-cape)

[Corfu](https://github.com/minad/corfu) enhances in-buffer completion by displaying a compact popup with current candidates, positioned either below or above the point. Candidates can be selected by navigating up or down.

[Cape](https://github.com/minad/cape), or Completion At Point Extensions, extends the capabilities of in-buffer completion. It integrates with Corfu or the default completion UI, by providing additional backends through completion-at-point-functions.

![](https://github.com/minad/corfu/blob/screenshots/popupinfo-dark.png?raw=true)

### Better Syntax highlighting (mod-treesit-auto)

The **mod-treesit-auto** module automatically installs and enables Tree-sitter major modes in Emacs 29 and later. If the Tree-sitter parser is unavailable or incompatible, it falls back to the original major mode.

Tree-sitter is an incremental parsing system introduced in Emacs 29 that delivers precise, high-performance syntax highlighting. It supports a wide range of programming languages, including Bash, C, C++, C#, CMake, CSS, Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML, Elisp, Lua, and many others.

### Efficient template expansion with snippets (mod-yasnippet and mod-yasnippet-snippets)

The **mod-yasnippet** configures [yasnippet](https://github.com/joaotavora/yasnippet), a package that provides a template system that enhances text editing by enabling users to define and use snippets, which are predefined templates of code or text. The user triggers snippet expansion by pressing the Tab key after typing an abbreviation, such as `if`. Upon pressing Tab, YASnippet replaces the abbreviation with the corresponding full template, allowing the user to fill in placeholders or fields within the expanded snippet.

The **mod-yasnippet-snippets** configures the [yasnippet-snippets](https://github.com/AndreaCrotti/yasnippet-snippets) package, which provides a comprehensive collection of bundled templates for numerous programming and markup languages, including C, C++, C#, Perl, Python, Ruby, SQL, LaTeX, HTML, CSS...

### Better undo/redo (mod-undo-fu and undo-fu-session)

The mod-undo-fu and undo-fu-session configure:
- [undo-fu](https://codeberg.org/ideasman42/emacs-undo-fu), a lightweight wrapper around Emacs' built-in undo system, providing more convenient undo/redo functionality while preserving access to the full undo history.
- [undo-fu-session](https://codeberg.org/ideasman42/emacs-undo-fu-session), which complements undo-fu by enabling the saving and restoration of undo history across Emacs sessions, even after restarting.

The default undo system in Emacs has two main issues that undo-fu fixes:

1. **Redo requires two steps**: To redo an action after undoing, you need to press a key twice, which can be annoying and inefficient.
2. **Accidental over-redo**: When redoing, it's easy to go too far back, past the point where you started the undo, which makes it hard to return to the exact state you wanted to restore.

If you use Evil mode, the `mod-undo-fu` module will replace Evil’s undo system with `undo-fu`.

### Vim Keybindings (mod-evil, mod-evil-snipe, mod-evil-surround, and evil-toggle-comment)

By default, [evil-mode](https://github.com/emacs-evil/evil) is enabled. (Users who prefer not to use Vim keybindings can easily disable it.)

Interesting default parameters:

- The default leader key is: `,`

The following packages are enabled alongside **evil-mode**:

- **mod-evil-toggle-comment**: Comment or uncomment text in Normal or Visual mode by pressing `gc`.

- **mod-evil-snipe**: Provides two-character motions for rapid navigation within text, similar to Evil’s built-in `f`/`F`/`t`/`T` commands, but with incremental highlighting of potential targets as you type. By default, `s` (forward) and `S` (backward) are bound to `evil-snipe-s` and `evil-snipe-S`, respectively. **Usage:** Pressing `s` in normal mode prompts you to type two characters, then jumps the cursor to the nearest matching occurrence while highlighting all matches incrementally.

- **mod-evil-surround**: Enables text surrounding in visual state using `S<textobject>` or `gS<textobject>`. For example, selecting text and pressing `S"` will wrap it in double quotes.

### Keybindings (mod-keybindings)

Defines the following key bindings:
- Increase or decrease the text scale using Ctrl combined with `+` or `-`.

### Code folding based on indentation (mod-outline-indent)

The `mod-outline-indent` module configures the [outline-indent](https://github.com/jamescherti/outline-indent.el) package, which provides `outline-indent-minor-mode`, a minor mode that enables code folding according to indentation levels.

In addition to code folding, *outline-indent* allows:
- Moving indented blocks up and down
- Indenting/unindenting to adjust indentation levels
- Inserting a new line with the same indentation level as the current line
- Move backward/forward to the indentation level of the current line
- and other features.

The `mod-outline-indent` module enabled `outline-indent-minor-mode` by default for YAML and Python files.

### Save History (mod-savehist)

The **mod-savehist** module configures **savehist**, a built-in Emacs feature that preserves the minibuffer history between sessions. It saves the history of inputs in the minibuffer, such as commands, search strings, and other prompts, to a file. This allows users to retain their minibuffer history across Emacs restarts.

### Save and Restore Cursor (mod-saveplace)

The **mod-saveplace** module enables `save-place-mode`, which makes Emacs remember the last location within a file when reopened. This facilitates resuming work exactly where it was left off.

### Auto Revert Buffer to Reflect Changes Made to the Underlying File on Disk (mod-autorevert)

Auto-revert is a feature that automatically updates the contents of a buffer to reflect changes made to the underlying file on disk.

### Persist and Restore Text Scale (mod-persist-text-scale)

The text scale can be adjusted by pressing **Ctrl** together with `+` to increase it (`text-scale-increase`) or `-` to decrease it (`text-scale-decrease`).

The mod-persist-text-scale module configures the [persist-text-scale](https://github.com/jamescherti/persist-text-scale.el) package, which ensures that all adjustments made with `text-scale-increase` and `text-scale-decrease` are persisted and restored across sessions. As a result, the text size in each buffer remains consistent, even after restarting Emacs.

This package also facilitates grouping buffers into categories, allowing buffers within the same category to share a consistent text scale. This ensures uniform font sizes when adjusting text scaling. By default:
- Each file-visiting buffer has its own independent text scale.
- Special buffers, identified by their buffer names, each retain their own text scale setting.
- All Dired buffers maintain the same font size, treating Dired as a unified "file explorer" where the text scale remains consistent across different buffers.

This category-based behavior can be further customized by assigning a function to the `persist-text-scale-buffer-category-function` variable. The function determines how buffers are categorized by returning a category identifier (string) based on the buffer's context. Buffers within the same category will share the same text scale.

### Automatically Remove Trailing Whitespace before Saving a Prog-mode Buffer

The **mod-stripspace** module configures the [stripspace](https://github.com/jamescherti/stripspace.el) Emacs package, which automatically removes trailing whitespace and blank lines at the end of the buffer when saving.

(Trailing whitespace refers to any spaces or tabs that appear at the end of a line, beyond the last non-whitespace character. These characters serve no purpose in the content of the file and can cause issues with version control, formatting, or code consistency. Removing trailing whitespace helps maintain clean, readable files.)

It also includes an optional feature (`stripspace-only-if-initially-clean`, disabled by default), which, when enabled, ensures that trailing whitespace is removed only if the buffer was initially clean. This prevents unintended modifications to buffers that already contain changes, making it useful for preserving intentional whitespace or avoiding unnecessary edits in files managed by version control.

### Recent files (mod-recentf)

Recentf maintains a list of recently accessed files, making it easier to reopen files you have worked on recently.

In addition to its built-in capabilities, the **mod-recentf** module provides the following enhancements:
- Inserts the current file at the beginning of the recent files list upon buffer switch.
- Cleans up the recent files list when quitting Emacs, prior to its automatic saving.
- Decrease recentf-mode verbosity by restricting its messages to the `*Messages*` buffer, preventing display in the minibuffer

### Other Modules Enabled by Default

- **mod-compile-angel**: Compile-angel speeds up Emacs by ensuring that all Elisp libraries are both byte-compiled and native-compiled.
- **mod-vim-tab-bar**: Enhances Emacs’ built-in tab bar with a minimalist, Vim-inspired design that automatically adapts to the current Emacs theme.
- **mod-markdown-mode**: The [markdown-mode](https://github.com/jrblevin/markdown-mode) package provides a major mode for Emacs for syntax highlighting, editing commands, and preview support for Markdown documents. It supports core Markdown syntax as well as extensions like GitHub Flavored Markdown (GFM).
- **mod-org**: Configures Org mode and Org Agenda, a major mode designed for organizing notes, planning, task management, and authoring documents using plain text with a simple and expressive markup syntax. It supports hierarchical outlines, TODO lists, scheduling, deadlines, time tracking, and exporting to multiple formats including HTML, LaTeX, PDF, and Markdown.
- **mod-org-appear**: Org-appear temporarily reveals normally hidden elements (such as emphasis markers, links, or entities) when the cursor enters them, and hides them again when the cursor leaves.
- **mod-aggressive-indent**: `aggressive-indent-mode` is a minor mode that Elisp code remains consistently indented. It automatically reindents after every modification, providing greater reliability than `electric-indent-mode`.

### Other Features

- Reduced clutter: A `var/` directory (e.g., `~/.emacs.d/var/`) is used to store all files that Emacs normally places in the base directory (e.g., `~/.emacs.d`). By default, Emacs stores configuration files, caches, backups, and other data within `~/.emacs.d`, which can accumulate over time and complicate management.

### Useful variables

#### Ellipsis
Change the default Ellipsis using the `lightemacs-ellipsis` variable, which defaults to `" ▼"`. This string used to indicate folded sections in `org-mode`, `outline-mode`, `outline-minor-mode`... This ellipsis appears at the end of a heading or section that has been collapsed. Modify the variable in your `~/.emacs/post-init.el` as follows:
```elisp
(setq lightemacs-ellipsis " ▼")
```

## Author and license

The *Lightemacs* project has been written by James Cherti and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2025 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program (in the .LICENSE file).

## Links

- [Lightemacs @GitHub](https://github.com/jamescherti/lightemacs)
