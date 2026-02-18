# Lightemacs - Fast and Lightweight Emacs Framework

The Lightemacs project is a **fast and lightweight Emacs framework** that uses modern Emacs features and essential packages, which enhance Emacs by improving completion, navigation, editing efficiency, and overall usability. It offers a minimalist alternative to frameworks like Doom Emacs or Spacemacs, allowing full customization to adapt the environment to your specific workflow.

<p align="center">
<img src="https://jamescherti.com/misc/lightemacs-m.png" width="40%" />
</p>

**Features:**

- Fast startup with optimized default settings.
- Minimalistic, distraction-free user interface.
- Modular design: Lightemacs provides a set of modules that can be enabled or disabled individually. Each module loads its packages only when needed, ensuring fast, deferred startup. By default, only essential modules are enabled.
- Lightemacs modules are loaded lazily: packages remain inactive until triggered by hooks, key mapping or file associations. This dramatically accelerates startup and minimizes loaded functions.

Lightemacs tweaks packages to improve performance. For example, adjusting the default parameters of Consult can eliminate perceived lag, and optimizing Show-Paren makes it more responsive, optimizing Evil mode (optional mode), and many others. Lightemacs saves time by preconfiguring these settings, allowing Emacs to feel faster and more efficient.

![](https://www.jamescherti.com/misc/screenshot-minimal-emacs-2.png)

Here are some of the modules that are enabled by default by the `le-flavor-essential` module:

- Enhanced Emacs completion and navigation capabilities (Consult, Embark, and Vertico, Marginalia, Orderless).
- Better sorting and filtering `M-x` (Vertico) and completion (Corfu) candidate lists with prescient.el, which adaptively ranks candidates based on selection frequency and recency, ensuring commonly used options or completions appear first.
- Ensure that all adjustments made with `text-scale-increase` and `text-scale-decrease` are persisted and restored across sessions.
- Improved undo/redo functionality with persistent undo history saved and restored across Emacs sessions, even after restarts.
- Preserve minibuffer history between sessions (savehist), persist and restore cursor position (saveplace), automatically update buffer contents to reflect changes in the underlying file on disk (autorevert), and maintain a list of recently accessed files (recentf).
- Additional file types: Markdown, Yaml, and Org.
- Configure Dired to display directories first and enable `dired-filter` to hide dotfiles, omit specified files, and exclude files listed in `.gitignore`. **Usage:** Use `C-c f` to toggle the filters on and off, showing or hiding the relevant files.
- Change the default Ellipsis using the `lightemacs-ellipsis` variable, which defaults to `" ▼"` String used to indicate folded sections in `org-mode`, `outline-mode`, `outline-minor-mode`...
- Save and restore the default theme using the `lightemacs-theme-name` variable.
- Functions for automatically detecting indentation offsets.
- Reduce clutter in the mode line by hiding or shortening the names of minor modes users rarely need to see (diminish.el). This makes the interface cleaner and allows you to focus only on the information that is actually useful.

Optionally, the following features can be enabled by loading additional Lightemacs modules:
- Press `C-=` to expand the selection step by step, from a word to a sentence, paragraph, or entire function, until it covers the text you want.
- **le-group-evil**: Provides Vim-style keybindings (Evil) with additional features, including commenting and uncommenting by pressing the `g` and `c` keys in sequence (`gc`), performing two-character searches with the `s` key as an alternative to `f`, and surrounding text in visual mode.
- **le-treesit-auto**: Better Syntax highlighting with Tree-sitter. (If the Tree-sitter parser is unavailable or incompatible, it falls back to the original major mode.)
- Ensure that all Elisp libraries are both byte-compiled and native-compiled to speed up Emacs.
- **le-yasnippet** and **le-yasnippet-snippets**: A template system that enhances text editing by enabling users to define and use snippets.
- **le-vterm**: A faster, more efficient terminal.
- **le-indent-bars**: Visual indentation guides, optimized for performance and customization.
- **le-paredit**: A package that assists in editing Lisp code by maintaining the structural integrity of s-expressions.
- And many others.

**What is the difference between Lightemacs and minimal-emacs.d?**

The Lightemacs project is built upon the [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) initialization files (`init.el` and `early-init.el`), allowing it to be configured identically to minimal-emacs.d while inheriting its high-quality default settings and efficient startup performance.

Unlike minimal-emacs.d, which provides a minimal and highly flexible Emacs configuration with only essential defaults, Lightemacs extends this foundation by enabling a curated set of modern features and optimizations out of the box. While minimal-emacs.d requires users to manually configure and enable most enhancements, Lightemacs activates performance improvements, advanced completion systems, persistent undo, snippet support, and additional filetype modes automatically, while still retaining full configurability and compatibility with minimal-emacs.d’s initialization files.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [Lightemacs - Fast and Lightweight Emacs Framework](#lightemacs---fast-and-lightweight-emacs-framework)
  - [Requirements](#requirements)
  - [Install Lightemacs](#install-lightemacs)
    - [Install Lightemacs into ~/.emacs.d](#install-lightemacs-into-emacsd)
    - [Alternative: Install Lightemacs into ~/.lightemacs.d](#alternative-install-lightemacs-into-lightemacsd)
  - [Update Lightemacs](#update-lightemacs)
  - [The hierarchy of Lightemacs files](#the-hierarchy-of-lightemacs-files)
    - [Files that must not be modified:](#files-that-must-not-be-modified)
    - [Files and directories you may edit:](#files-and-directories-you-may-edit)
  - [Example configurations](#example-configurations)
  - [Customizations](#customizations)
    - [Never modify init.el and early-init.el. Modify these instead...](#never-modify-initel-and-early-initel-modify-these-instead)
    - [Package Manager Selection](#package-manager-selection)
      - [Supported package managers](#supported-package-managers)
      - [Configuration Example for the package manager](#configuration-example-for-the-package-manager)
    - [How to enable the menu-bar, the tool-bar, dialogs, the contextual menu, and tooltips?](#how-to-enable-the-menu-bar-the-tool-bar-dialogs-the-contextual-menu-and-tooltips)
  - [Modules Enabled by Default](#modules-enabled-by-default)
    - [Enabled by Default: Default theme (le-theme)](#enabled-by-default-default-theme-le-theme)
    - [Enabled by Default: Better minibuffer and navigation (le-consult, le-embark, and le-vertico, le-marginalia, le-orderless, le-consult-dir)](#enabled-by-default-better-minibuffer-and-navigation-le-consult-le-embark-and-le-vertico-le-marginalia-le-orderless-le-consult-dir)
    - [Enabled by Default: Better completion (le-corfu and le-cape)](#enabled-by-default-better-completion-le-corfu-and-le-cape)
    - [Enabled by Default: Better sorting and ordering (le-prescient, le-corfu-prescient, and le-vertico-prescient)](#enabled-by-default-better-sorting-and-ordering-le-prescient-le-corfu-prescient-and-le-vertico-prescient)
    - [Enabled by Default: Enhanced File Management (le-dired and le-dired-filter)](#enabled-by-default-enhanced-file-management-le-dired-and-le-dired-filter)
    - [Enabled by Default: Better undo/redo (le-undo-fu and undo-fu-session)](#enabled-by-default-better-undoredo-le-undo-fu-and-undo-fu-session)
    - [Enabled by Default: Keybindings (le-keybindings)](#enabled-by-default-keybindings-le-keybindings)
    - [Enabled by Default: Code folding based on indentation (le-outline-indent)](#enabled-by-default-code-folding-based-on-indentation-le-outline-indent)
    - [Enabled by Default: Save History (le-savehist)](#enabled-by-default-save-history-le-savehist)
    - [Enabled by Default: Save and Restore Cursor (le-saveplace)](#enabled-by-default-save-and-restore-cursor-le-saveplace)
    - [Enabled by Default: Auto Revert Buffer to Reflect Changes Made to the Underlying File on Disk (le-autorevert)](#enabled-by-default-auto-revert-buffer-to-reflect-changes-made-to-the-underlying-file-on-disk-le-autorevert)
    - [Enabled by Default: Persist and Restore Text Scale (le-persist-text-scale)](#enabled-by-default-persist-and-restore-text-scale-le-persist-text-scale)
    - [Enabled by Default: A better way to rename or delete files (le-bufferfile)](#enabled-by-default-a-better-way-to-rename-or-delete-files-le-bufferfile)
    - [Enabled by Default: Recent files (le-recentf)](#enabled-by-default-recent-files-le-recentf)
    - [Enabled by Default: Detect indentation offset (le-dtrt-indent)](#enabled-by-default-detect-indentation-offset-le-dtrt-indent)
    - [The built-in on-the-fly syntax checker (le-flymake)](#the-built-in-on-the-fly-syntax-checker-le-flymake)
    - [Other Modules Enabled by Default](#other-modules-enabled-by-default)
  - [Modules Disabled by Default](#modules-disabled-by-default)
    - [Disabled by default: le-group-evil (Vim Keybindings)](#disabled-by-default-le-group-evil-vim-keybindings)
    - [Disabled by default: Automatically Remove Trailing Whitespace before Saving a Prog-mode Buffer](#disabled-by-default-automatically-remove-trailing-whitespace-before-saving-a-prog-mode-buffer)
    - [Disabled by default: le-treesit-auto (better syntax highlighting)](#disabled-by-default-le-treesit-auto-better-syntax-highlighting)
    - [Disabled by default: Runs code formatters asynchronously (le-apheleia)](#disabled-by-default-runs-code-formatters-asynchronously-le-apheleia)
    - [Disabled by default: Persisting and Restoring all buffers, windows/split, tab-bar, frames... (le-easysession)](#disabled-by-default-persisting-and-restoring-all-buffers-windowssplit-tab-bar-frames-le-easysession)
    - [Disabled by default: Efficient template expansion with snippets (le-yasnippet and le-yasnippet-snippets)](#disabled-by-default-efficient-template-expansion-with-snippets-le-yasnippet-and-le-yasnippet-snippets)
    - [Disabled by default: An alternative terminal (le-vterm)](#disabled-by-default-an-alternative-terminal-le-vterm)
    - [Disabled by default: Better Elisp editing (le-group-emacs-lisp)](#disabled-by-default-better-elisp-editing-le-group-emacs-lisp)
    - [Disabled by default: Indentation bars (le-indent-bars)](#disabled-by-default-indentation-bars-le-indent-bars)
    - [Enabled by Default: Expand Region (le-expand-region)](#enabled-by-default-expand-region-le-expand-region)
    - [Other modules disabled by default](#other-modules-disabled-by-default)
    - [Interesting builtin modules that Lightemacs configures](#interesting-builtin-modules-that-lightemacs-configures)
  - [Other Features](#other-features)
  - [Useful variables, functions, and macros](#useful-variables-functions-and-macros)
    - [Useful variables](#useful-variables)
      - [Ellipsis](#ellipsis)
      - [Cycling](#cycling)
      - [Other variables](#other-variables)
    - [Useful functions and macros](#useful-functions-and-macros)
  - [Author and license](#author-and-license)
  - [Links](#links)

<!-- markdown-toc end -->

## Requirements

- Emacs >= 29.1
- Git
- Optional: [ripgrep](https://github.com/BurntSushi/ripgrep) and [fd](https://github.com/sharkdp/fd) (Useful for `consult-ripgrep` and `consult-fd` commands)

## Install Lightemacs

- **Important:** Ensure that the `~/.emacs` and `~/.emacs.el` files do not exist. These files cause Emacs to ignore `~/.emacs.d/init.el`. This behavior is due to the way Emacs searches for initialization files ([more information](https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html#Find-Init)). **Simply delete the *~/.emacs* and *~/.emacs.el* files avoid this issue.**
- **Debug:** If a package or any other functionality is not working as expected, start Emacs with `emacs --debug-init` to enable debug mode and obtain the backtrace.
- **Prerequisite:** git

### Install Lightemacs into ~/.emacs.d

Execute the following command install this repository into `~/.emacs.d`:
```
git clone --depth 1 https://github.com/jamescherti/lightemacs ~/.emacs.d
```

### Alternative: Install Lightemacs into ~/.lightemacs.d

To install *Lightemacs* in a non-default directory, use the `--init-directory` Emacs option to specify your desired configuration path. For example, to install *Lightemacs* in `~/.lightemacs.d/`, follow these steps:

1. Clone the repository into `~/.lightemacs.d/` using:
   ```
   git clone --depth 1 https://github.com/jamescherti/lightemacs ~/.lightemacs.d
   ```

2. Start Emacs with the new configuration directory:
   ```
   emacs --init-directory ~/.lightemacs.d
   ```

## Update Lightemacs

To update your Lightemacs configuration and its submodules, run the following commands:
```
git -C ~/.emacs.d pull
```

## The hierarchy of Lightemacs files

If you install Lightemacs in `~/.emacs.d/`, the directory structure is as follows:

### Files that must not be modified:

- `~/.emacs.d/var/`: Contains all files generated dynamically by Emacs and its plugins. Lightemacs redirects such files here to prevent cluttering `~/.emacs.d/`.
- `~/.emacs.d/lisp/lightemacs/`: Contains the modules and libraries used by Lightemacs (do not modify these files).
- `~/.emacs.d/early-init.el` and `~/.emacs.d/init.el`: Initialization files (do not modify these files).

### Files and directories you may edit:

- `~/.emacs.d/lisp/local/config.el`: The main configuration file, where you may adjust settings or install packages.
- `~/.emacs.d/lisp/local/modules/`: A directory for your personal modules.

*(Files and directories intended for user modification, such as `~/.emacs.d/lisp/local/config.el` or any files within `~/.emacs.d/lisp/local/`, are not tracked by Git.)*

## Example configurations

The `~/.emacs.d/lisp/local/config.el` file serves as the primary configuration for Lightemacs, allowing settings to be adjusted and additional packages to be installed. (If the configuration directory is changed, for example to `~/.lightemacs.d/`, the main configuration file will then be located at `~/.lightemacs.d/config.el`.)

Example 1: The default `config.el` configuration only contains [le-flavor-essential](https://github.com/jamescherti/lightemacs/blob/main/lisp/lightemacs/modules/le-flavor-essential.el):

```elisp
;;; config.el --- Lightemacs Config -*- lexical-binding: t; -*-

(defun lightemacs-user-init ()
  "This function is executed right before modules are loaded."
  (setq lightemacs-modules '(le-flavor-essential)))

```

Example 2: The configuration above does not include Vim Keybindings, providing standard Emacs behavior for users who do not use Evil-mode. To enable Vim Keybindings (Evil-mode), add [le-group-evil](https://github.com/jamescherti/lightemacs/blob/main/lisp/lightemacs/modules/le-group-evil.el) to the configuration:

```elisp
;;; config.el --- Lightemacs Config -*- lexical-binding: t; -*-

(defun lightemacs-user-init ()
  "This function is executed right before modules are loaded."
  (setq lightemacs-modules '(le-flavor-essential

                             ;; Vim keybindings
                             le-group-evil)))
```

Example 3: This configuration includes most of modules ([le-flavor-big](https://github.com/jamescherti/lightemacs/blob/main/lisp/lightemacs/modules/le-flavor-big.el)):

```elisp
;;; config.el --- Lightemacs Config -*- lexical-binding: t; -*-

(defun lightemacs-user-init ()
  "This function is executed right before modules are loaded."
  (setq lightemacs-modules '(le-flavor-big)))
```

## Customizations

### Never modify init.el and early-init.el. Modify these instead...

**The `init.el` and `early-init.el` files should never be modified directly** because they are intended to be managed by Git during an update.

Modify `~/.emacs.d/lisp/local/config.el` instead. This file is loaded after `init.el` but before the Lightemacs modules are initialized. It is intended for supplementary configurations or package setups.

Always begin your `config.el` file with the following header to prevent them from being byte-compiled and to activate lexical binding:
```emacs-lisp
;;; config.el --- Configuration -*- no-byte-compile: t; lexical-binding: t; -*-
```

*(Only if you know what you're doing: Removing `no-byte-compile: t;` from your init and `config.el` files allows Emacs to compile them, improving load and execution speed. However, if you do so, you may need to add required dependencies. For example, if you're using `use-package`, add `(require 'use-package)` at the top of initialization files to ensure all necessary `use-package` variables and functions are loaded.)*

Here is an example of `config.el` file:
```elisp
;;; config.el --- Configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(setq lightemacs-modules '(le-flavor-essential))

;; (defun lightemacs-user-init ()
;;   "This function is executed right before modules are loaded."
;;   (message "Before modules"))
```

(The Lightemacs project extends the [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) initialization files, enabling configuration in the same manner as *minimal-emacs.d* and supporting the same `pre-` and `post-` initialization files: `pre-init.el`, `post-init.el`, `pre-early-init.el`, and `post-early-init.el`. The only distinction is that, in Lightemacs, these files must be placed in the `~/.emacs.d/lisp/local/` directory, alongside `config.el`. Configuration in Lightemacs is typically done through `config.el`.)

### Package Manager Selection

Lightemacs allows choosing the package manager through the `lightemacs-package-manager` variable. This variable determines the underlying system used for installation, dependency resolution, and configuration of packages via `lightemacs-module-package`.

By default, `lightemacs-package-manager` is set to `'use-package`, which uses the built-in `package.el` together with `use-package`.

#### Supported package managers

- **`'use-package`** (default): Uses Emacs’ native `package.el` and the `use-package` macro. This backend is suitable for users who prefer relying on the standard Emacs ecosystem, without additional package management layers. To update all packages, run `M-x package-upgrade-all`

- WORK IN PROGRESS: **`'straight`**: Uses `straight.el`, providing fully reproducible builds, precise control over package recipes, and integration with `use-package` via the `:straight` keyword. This is ideal for users who need deterministic environments or advanced package customization. To update all packages, run `M-x straight-pull-all`; to rebuild all packages, run `M-x straight-rebuild-all`.

- WORK IN PROGRESS: **`'elpaca`**: Leverages `elpaca` for asynchronous, dependency-aware package management. Elpaca simplifies recipe handling and integrates with `use-package` through the `:elpaca` keyword.

#### Configuration Example for the package manager

Add the following to `~/.emacs.d/lisp/local/config.el`:

```elisp
(setq lightemacs-package-manager 'use-package)
```

This guarantees that the built-in `use-package` will manage package installation, loading, and configuration according to the semantics of the selected package manager.

### How to enable the menu-bar, the tool-bar, dialogs, the contextual menu, and tooltips?

The Lightemacs project is based on the [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) initialization files, which means it can be configured in exactly the same way as minimal-emacs.d.

**Note:** Enabling the tool-bar or menu-bar may slightly increase your startup time.

To customize your Emacs setup to include various user interface elements, you can use the following settings in your ``~/.emacs.d/pre-early-init.el``:

``` emacs-lisp
(setq minimal-emacs-ui-features '(context-menu tool-bar menu-bar dialogs tooltips))
```

These settings control the visibility of dialogs, context menus, toolbars, menu bars, and tooltips.

## Modules Enabled by Default

Here are the modules that are enabled by default by the flavor that is enabled by default (`le-flavor-essential`):

### Enabled by Default: Default theme (le-theme)

The `le-theme` module loads the default theme. It can be configured via the `lightemacs-theme-name` variable. To customize this theme, modify the variable in your `~/.emacs.d/lisp/local/config.el` as follows:

```emacs-lisp
;; Default theme to load during initialization, if available. Set to nil to
;; disable loading a theme at startup.
(setq lightemacs-theme-name 'doom-one)

;; Theme package to install and use for `lightemacs-theme-name'. Set to
;; nil to disable installing this package at startup.
(setq lightemacs-theme-package 'doom-themes)
```

Here are examples of alternative built-in themes:
```emacs-lisp
(setq lightemacs-theme-name 'modus-operandi)
(setq lightemacs-theme-name 'modus-vivendi)
(setq lightemacs-theme-name 'tango)
(setq lightemacs-theme-name 'tango-dark)
```

### Enabled by Default: Better minibuffer and navigation (le-consult, le-embark, and le-vertico, le-marginalia, le-orderless, le-consult-dir)

Vertico, Consult, Marginalia, and Embark collectively enhance Emacs' completion and navigation capabilities:
- **le-vertico** configures [Vertico](https://github.com/minad/vertico), a vertical completion interface, making it easier to navigate and select from completion candidates (e.g., when `M-x` is pressed).
- **le-consult** configures [Consult](https://github.com/minad/consult), a suite of commands for efficient searching, previewing, and interacting with buffers, file contents, and more, improving various tasks. *(Try `M-x consult-rg` to search your project using ripgrep, or `M-x consult-fd` to quickly locate files in your workspace.)* This module configures: consult, consult-xref, and consult-imenu. This module also provides improved default settings for `consult-fd`, `consult-find`, `consult-grep`, and `consult-ripgrep`, enhancing file and text search performance and usability.
- **le-embark** configures [Embark](https://github.com/oantolin/embark), a tool that provides context-sensitive actions and quick access to commands based on the current selection, further improving user efficiency and workflow within Emacs. Together, they create a cohesive and powerful environment for managing completions and interactions. *(While searching with `M-x consult-rg` or `M-x consult-fd`, you can use `M-x embark-export` to act on the search results. This allows you to export the matches to another buffer, perform batch operations, or open multiple files at once, providing an excellent way to manipulate and navigate large sets of results.)*
- **le-marginalia** configures [Marginalia](https://github.com/minad/marginalia), a package that enriches minibuffer completions with contextual annotations. It enhances Vertico by adding rich annotations to completion candidates, such as file sizes, documentation, or metadata.
- **le-orderless**: Enable flexible, unordered matching (Orderless) for Vertico. This allows you to type multiple parts of a candidate in any order, making it easier to find functions, variables, or files even if you only remember fragments. It speeds up navigation by removing the need to type exact prefixes. For example, typing "main test" matches "test_main.py", "read me" matches "README.md".
- **le-consult-dir**: Configures the [consult-dir](https://github.com/karthink/consult-dir) package which extends the Consult framework by providing convenient ways to quickly switch to and insert directory paths. It supports switching to recent directories, project roots (if available), bookmarks, or TRAMP hosts. Similar to tools like *autojump* or *fasd*, it enables fast directory-jumping in Emacs. The command `M-x consult-dir` is globally available on `C-x C-d`, and is also bound within the minibuffer completion map to allow directory switching while completing file paths. Additionally, `consult-dir-jump-file` is bound to `C-x C-j` in the minibuffer, enabling direct navigation to files within a chosen directory, thereby extending the file-jumping workflow directly from minibuffer-based commands. (By default, this module also integrates with `fasd`, providing directory candidates from it when `fasd` is installed.)

![](https://github.com/minad/consult/blob/screenshots/consult-grep.gif?raw=true)

Keybindings for Consult:

- `C-c M-x` → `consult-mode-command`
  Invoke a command specific to the current major mode using Consult.

- `C-c h` → `consult-history`
  Browse and select from the minibuffer history.

- `C-c m` → `consult-man`
  Access Unix manual pages interactively.

- `C-c i` → `consult-info`
  Search and navigate Info documentation.

- `C-c r` → `consult-recent-file`
  `consult-recent-file` is a Consult command that uses the `recentf` list to let
  you quickly open recently visited files via minibuffer completion.

- `Info-search` remapped → `consult-info`
  Redirects standard Info search to use Consult.

- `C-x M-:` → `consult-complex-command`
  Browse and execute complex commands from history.

- `C-x b` → `consult-buffer`
  Switch between buffers interactively.

- `C-x 4 b` → `consult-buffer-other-window`
  Open a buffer in another window.

- `C-x 5 b` → `consult-buffer-other-frame`
  Open a buffer in a new frame.

- `C-x t b` → `consult-buffer-other-tab`
  Open a buffer in a new tab (if tab support is enabled).

- `C-x r b` → `consult-bookmark`
  Jump to a bookmark using Consult’s interface.

- `C-x p b` → `consult-project-buffer`
  Switch buffers within the current project.

- `M-#` → `consult-register-load`
  Load a value from a register.

- `M-'` → `consult-register-store`
  Store a value into a register.

- `C-M-#` → `consult-register`
  Access all register operations interactively.

- `M-y` → `consult-yank-pop`
  Browse and yank from kill-ring entries.

- `M-g e` → `consult-compile-error`
  Navigate compilation errors.

- `M-g f` → `consult-flymake`
  Jump to Flymake diagnostics.

- `M-g g` / `M-g M-g` → `consult-goto-line`
  Jump to a specific line number.

- `M-g o` → `consult-outline`
  Navigate headings in the current buffer.

- `M-g m` → `consult-mark`
  Jump to local marks.

- `M-g k` → `consult-global-mark`
  Jump to global marks across buffers.

- `M-g i` → `consult-imenu`
  Jump to an Imenu entry in the current buffer.

- `M-g I` → `consult-imenu-multi`
  Jump to Imenu entries across multiple buffers.

- `M-s d` → `consult-find`
  Locate files in the current project or directory.

- `M-s c` → `consult-locate`
  Search files using `locate` database.

- `M-s g` → `consult-grep`
  Search for a string using `grep`.

- `M-s G` → `consult-git-grep`
  Perform `git grep` in the repository.

- `M-s p` → `consult-fd`
  Perform `fd` searches.

- `M-s r` → `consult-ripgrep`
  Perform `ripgrep` searches.

- `M-s l` → `consult-line`
  Jump to a line matching a query in the buffer.

- `M-s L` → `consult-line-multi`
  Search for multiple lines simultaneously.

- `M-s k` → `consult-keep-lines`
  Keep only lines matching a pattern.

- `M-s u` → `consult-focus-lines`
  Focus on lines matching a pattern.

- `M-s e` → `consult-isearch-history`
  Browse previous search strings from `isearch`.

**Isearch Mode Specific Bindings:**

- `M-e`, `M-s e` → `consult-isearch-history`
  Access previous search history during incremental search.

- `M-s l` → `consult-line`
  Jump to matching lines while in `isearch`.

- `M-s L` → `consult-line-multi`
  Search multiple lines during `isearch`.

**Minibuffer Specific Bindings:**

- `M-s`, `M-r` → `consult-history`
  Access minibuffer history while inside the minibuffer.

### Enabled by Default: Better completion (le-corfu and le-cape)

- [Corfu](https://github.com/minad/corfu) enhances in-buffer completion by displaying a compact popup with current candidates, positioned either below or above the point. Candidates can be selected by navigating up or down. By default, Corfu shows completions automatically without requiring the user to press Tab. To make Corfu complete only when the user presses Tab, add the following to `~/.emacs.d/lisp/local/config.el`:
  ```emacs-lisp
  ;; By default, Corfu shows completions automatically without requiring the user
  ;; to press Tab. To make Corfu complete only when the user presses Tab:
  ;;
  ;; To make Corfu complete only when the user presses Tab:
  (setq corfu-auto nil)
  ```

- [Cape](https://github.com/minad/cape), or Completion At Point Extensions, extends the capabilities of in-buffer completion. It integrates with Corfu or the default completion UI, by providing additional backends through completion-at-point-functions.

![](https://github.com/minad/corfu/blob/screenshots/popupinfo-dark.png?raw=true)

### Enabled by Default: Better sorting and ordering (le-prescient, le-corfu-prescient, and le-vertico-prescient)

The **le-prescient** configures [prescient.el](https://github.com/radian-software/prescient.el) is a library for sorting and filtering lists of candidates, such as those presented by packages like Vertico or Corfu.

The main benefit of *prescient.el* is that it adaptively orders candidates based on both frequency and recency of selection, making frequently used options appear first without sacrificing predictable filtering results.

- Vertico and Prescient (le-vertico-prescient): When prescient.el is used with Vertico, prescient.el enhances minibuffer completion by dynamically reordering candidates based on frequency and recency, making it faster to select commonly used options while preserving consistent, predictable filtering. **Example:** When running `M-x` and repeatedly selecting the command `compile`, prescient.el will place `compile` near the top of the Vertico minibuffer list in future sessions, reducing the need to type its full name.
- Corfu and Prescient (le-corfu-prescient): When prescient.el is used with Corfu, prescient.el improves both in-buffer completions and pop-up completion menus by making candidate ordering more predictable and adaptive to recent usage, thus speeding up repeated selections. **Example:** If you frequently choose the completion `printf` when editing C code, prescient.el will gradually move `printf` toward the top of the list whenever similar candidates are offered, reducing the number of keystrokes needed to select it.

### Enabled by Default: Enhanced File Management (le-dired and le-dired-filter)

* **le-dired**: Configures Dired to display directories before files and to omit specified files and directories (e.g., `.git`, `*.pyc`, `*.o`). Customizations:
  - The parent directory entry (`..`) can be hidden by setting the variable `lightemacs-dired-omit-parent-directory` to `t`. (The `..` entry is redundant since pressing the `-` key navigates to the parent directory.)
  - The variable `lightemacs-dired-hide-details-mode`, enabled by default, hides file details such as permissions, sizes, and modification dates.
- **le-dired-filter**: Uses `dired-filter` to hide files, including dotfiles, omitted files, and files ignored by Git.

The **le-dired-filter** module only enables `dired-filter-by-omit`:

```emacs-lisp
;; By default, `dired-filter-by-omit' excludes "."
(setq lightemacs-dired-filter-setup-hook '(dired-filter-by-omit))
```

To add additional filters, include them in your `~/.emacs.d/lisp/local/config.el`. For example:

```emacs-lisp
(setq lightemacs-dired-filter-setup-hook '(dired-filter-by-omit
                                           ;; Hide files ignored by Git
                                           dired-filter-by-git-ignored
                                           ;; Hide dotfiles
                                           dired-filter-by-dot-files))
```

This setup keeps your Dired buffer clean by showing only relevant and tracked files.

The `dired-filter-by-omit` filter can be extended to conceal additional entries. For instance, it can be configured to hide `.`, `..`, and `.git` directories:
```emacs-lisp
(setq dired-omit-files "^\.$\\|^\\.\\.$\\|^\\.git$")
```

(Hiding `..` is acceptable since the `-` key provides a way to navigate to the parent directory.)

### Enabled by Default: Better undo/redo (le-undo-fu and undo-fu-session)

The le-undo-fu and undo-fu-session configure:
- [undo-fu](https://codeberg.org/ideasman42/emacs-undo-fu), a lightweight wrapper around Emacs' built-in undo system, providing more convenient undo/redo functionality while preserving access to the full undo history.
- [undo-fu-session](https://codeberg.org/ideasman42/emacs-undo-fu-session), which complements undo-fu by enabling the saving and restoration of undo history across Emacs sessions, even after restarting.

The default undo system in Emacs has two main issues that undo-fu fixes:

1. **Redo requires two steps**: To redo an action after undoing, you need to press a key twice, which can be annoying and inefficient.
2. **Accidental over-redo**: When redoing, it's easy to go too far back, past the point where you started the undo, which makes it hard to return to the exact state you wanted to restore.

If you use Evil mode, the `le-undo-fu` module will replace Evil’s undo system with `undo-fu`.

### Enabled by Default: Keybindings (le-keybindings)

Defines the following key bindings:
- Increase or decrease the text scale using Ctrl combined with `+` or `-`.

### Enabled by Default: Code folding based on indentation (le-outline-indent)

The `le-outline-indent` module configures the [outline-indent](https://github.com/jamescherti/outline-indent.el) package, which provides `outline-indent-minor-mode`, a minor mode that enables code folding according to indentation levels.

In addition to code folding, *outline-indent* allows:
- Moving indented blocks up and down
- Indenting/unindenting to adjust indentation levels
- Inserting a new line with the same indentation level as the current line
- Move backward/forward to the indentation level of the current line
- and other features.

The `le-outline-indent` module can be enabled using `M-x outline-indent-minor-mode`.

The following example can be added to the `~/.emacs.d/lisp/local/config.el` file to automatically enable `outline-indent-minor-mode` for YAML and Python files:
```emacs-lisp
(with-eval-after-load 'le-outline-indent
  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)

  (add-hook 'python-mode-hook #'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode))
```

### Enabled by Default: Save History (le-savehist)

The **le-savehist** module configures **savehist**, a built-in Emacs feature that preserves the minibuffer history between sessions. It saves the history of inputs in the minibuffer, such as commands, search strings, and other prompts, to a file. This allows users to retain their minibuffer history across Emacs restarts.

### Enabled by Default: Save and Restore Cursor (le-saveplace)

The **le-saveplace** module enables `save-place-mode`, which makes Emacs remember the last location within a file when reopened. This facilitates resuming work exactly where it was left off.

(When `scroll-conservatively` is set to 101 or higher, Emacs may position the point near the bottom of the window, which can be disorienting. The **le-saveplace** module addresses this by automatically recentering the window after `save-place` restores the cursor position, ensuring that the point is more centrally located even when `scroll-conservatively` is high.)

### Enabled by Default: Auto Revert Buffer to Reflect Changes Made to the Underlying File on Disk (le-autorevert)

Auto-revert is a feature that automatically updates the contents of a buffer to reflect changes made to the underlying file on disk.

To suppress minibuffer messages when Auto Revert reverts a buffer, add the following line to `~/.emacs.d/lisp/local/config.el`:

```emacs-lisp
;; To suppress minibuffer messages when Auto Revert reverts a buffer
(setq auto-revert-verbose nil)
```

### Enabled by Default: Persist and Restore Text Scale (le-persist-text-scale)

The text scale can be adjusted by pressing **Ctrl** together with `+` to increase it (`text-scale-increase`) or `-` to decrease it (`text-scale-decrease`).

The le-persist-text-scale module configures the [persist-text-scale](https://github.com/jamescherti/persist-text-scale.el) package, which ensures that all adjustments made with `text-scale-increase` and `text-scale-decrease` are persisted and restored across sessions. As a result, the text size in each buffer remains consistent, even after restarting Emacs.

This package also facilitates grouping buffers into categories, allowing buffers within the same category to share a consistent text scale. This ensures uniform font sizes when adjusting text scaling. By default:
- Each file-visiting buffer has its own independent text scale.
- Special buffers, identified by their buffer names, each retain their own text scale setting.
- All Dired buffers maintain the same font size, treating Dired as a unified "file explorer" where the text scale remains consistent across different buffers.

This category-based behavior can be further customized by assigning a function to the `persist-text-scale-buffer-category-function` variable. The function determines how buffers are categorized by returning a category identifier (string) based on the buffer's context. Buffers within the same category will share the same text scale.

### Enabled by Default: A better way to rename or delete files (le-bufferfile)

The **le-bufferfile** configures [bufferfile](https://github.com/jamescherti/bufferfile.el), package that provides helper functions to delete, rename, or copy buffer files:
- `M-x bufferfile-rename`: Renames the file visited by the current buffer, ensures that the destination directory exists, and updates the buffer name for all associated buffers, including clones/indirect buffers. It also ensures that buffer-local features referencing the file, such as Eglot or dired buffers, are correctly updated to reflect the new file name.
- `M-x bufferfile-delete`: Delete the file associated with a buffer and kill all buffers visiting the file, including clones/indirect buffers.
- `M-x bufferfile-copy`: Ensures that the destination directory exists and copies the file visited by the current buffer to a new file.

The functions above also ensure that any modified buffers are saved prior to executing operations like renaming, deleting, or copying.

To replace the default *dired* rename command with `bufferfile-rename`, add the following to your `~/.emacs.d/lisp/local/config.el` file:

```emacs-lisp
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "R") #'bufferfile-dired-do-rename)

  ;; For users of `evil-mode', uncomment the following sexp:
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key 'normal dired-mode-map (kbd "R") #'bufferfile-dired-do-rename))
  )
```

To make *bufferfile* use version control when renaming or deleting files, add the following to your `~/.emacs.d/lisp/local/config.el` file:

```emacs-lisp
;; Use version control when renaming or deleting files with `bufferfile-rename'
(setq bufferfile-use-vc t)
```

### Enabled by Default: Recent files (le-recentf)

Recentf maintains a list of recently accessed files, making it easier to reopen files you have worked on recently.

In addition to its built-in capabilities, the **le-recentf** module provides the following enhancements:
- Inserts the current file at the beginning of the recent files list upon buffer switch.
- Cleans up the recent files list when quitting Emacs, prior to its automatic saving.
- Cleans up and saves the recentf list every `lightemacs-recentf--auto-save-timer-interval` seconds (default: 550).
- Decrease recentf-mode verbosity by restricting its messages to the `*Messages*` buffer, preventing display in the minibuffer

Key mapping:

- Use `C-f` to invoke `recentf`.

### Enabled by Default: Detect indentation offset (le-dtrt-indent)

The **le-dtrt-indent** module configures the [dtrt-indent](https://github.com/jscheid/dtrt-indent) package, which provides functions to automatically detect the indentation offset, defined as the number of spaces or the tab width used for code indentation.

To make `dtrt-indent` display a message whenever it adjusts the indentation offset, add the following line to your `~/.emacs.d/lisp/local/config.el`:

```emacs-lisp
(setq dtrt-indent-verbosity 1)
```

The `le-dtrt-indent` module allows controlling automatic indentation detection via:
- `lightemacs-dtrt-indent-inhibit`: When non-nil, disables automatic indentation detection in the current buffer.
- `lightemacs-dtrt-indent-excluded-modes`: List of major modes where `dtrt-indent` should not run. For example:
  ```emacs-lisp
  ;; Exclude Python and Yaml/Ansible
  (setq lightemacs-dtrt-indent-excluded-modes '(python-mode
                                                python-ts-mode
                                                yaml-mode
                                                yaml-ts-mode
                                                ansible-mode))
  ```

### The built-in on-the-fly syntax checker (le-flymake)

The **le-flymake** module configures Flymake, a built-in on-the-fly syntax checking tool that analyzes source code buffers in the background and highlights errors or warnings as you type. It invokes external syntax checkers or compilers asynchronously and annotates the buffer with diagnostic messages, which can be navigated using dedicated commands. Unlike language servers, Flymake itself does not perform analysis but provides a flexible framework that integrates with various backends, making it lightweight, extensible, and adaptable across different programming languages.

By default, the **le-flymake** module enables Flymake automatically in `prog-mode` and `text-mode`.

The **le-flymake** defines the following keybindings for navigating Flymake diagnostics:

- `M-g n`: Move to the **next** error or warning in the current buffer.
- `M-g p`: Move to the **previous** error or warning in the current buffer.

These keys are bound in `flymake-mode-map`, so they are active only when `flymake-mode` is enabled. The mnemonic follows Emacs’ convention: `M-g` is the **goto** prefix, and `n`/`p` indicate **next** and **previous** respectively.

(Additionally, the **le-flymake** module enhances Flymake for Emacs Lisp by ensuring that `elisp-flymake-byte-compile-load-path` includes all directories in the current `load-path`. This allows Flymake to locate and check all installed Emacs Lisp files during on-the-fly byte-compilation, improving accuracy of syntax checking in Emacs Lisp buffers.)

### Other Modules Enabled by Default

- **le-outline**: Update the ellipsis in `outline-minor-mode` using the `lightemacs-ellipsis` variable. The `outline-minor-mode` enabled code folding in programming and can be configured by adding the following to the `~/.emacs.d/lisp/local/config.el` file:
  ```emacs-lisp
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  ```

  You can also, as an alternative to prog-mode-hook, add hook to specific modes:
  ```emacs-lisp
  (add-hook 'grep-mode-hook #'outline-minor-mode)
  (add-hook 'conf-mode-hook #'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'js-mode-hook #'outline-minor-mode)
  (add-hook 'js-ts-mode-hook #'outline-minor-mode)
  (add-hook 'markdown-ts-mode-hook #'outline-minor-mode)
  (add-hook 'markdown-mode-hook #'outline-minor-mode)
  ```
- **le-vim-tab-bar**: Enhances Emacs’ built-in tab bar with a minimalist, Vim-inspired design that automatically adapts to the current Emacs theme.
- **le-wgrep**: The [wgrep](https://github.com/mhayashi1120/Emacs-wgrep) (Writable Grep) package enables you to convert a grep, consult-ripgrep, or Embark Export buffers into an editable interface. It allows in-place modification of matched lines within the results buffer, which can then be propagated back to the corresponding files upon confirmation. This facilitates precise, bulk edits across multiple files efficiently, eliminating the need to open each file individually, and effectively transforms the grep results buffer into a controlled, multi-file editing environment.
- **le-group-markdown**: Configures the [markdown-mode](https://github.com/jrblevin/markdown-mode) package, which provides a major mode for Emacs for syntax highlighting, editing commands, and preview support for Markdown documents. It supports core Markdown syntax as well as extensions like GitHub Flavored Markdown (GFM). This group also configures [markdown-toc](https://github.com/ardumont/markdown-toc).
- **le-org**: Configures Org mode and Org Agenda, a major mode designed for organizing notes, planning, task management, and authoring documents using plain text with a simple and expressive markup syntax. It supports hierarchical outlines, TODO lists, scheduling, deadlines, time tracking, and exporting to multiple formats including HTML, LaTeX, PDF, and Markdown.
- **le-org-appear**: Org-appear temporarily reveals normally hidden elements (such as emphasis markers, links, or entities) when the cursor enters them, and hides them again when the cursor leaves.
- **le-winner**: Track changes in the window configuration, allowing undoing actions such as closing windows using `winner-undo`.
- **le-elec-pair**: Automatically insert matching delimiters (), {}...
- **le-paren**: `show-paren-mode` allows one to see matching pairs of parentheses and other characters. When point is on the opening character of one of the paired characters, the other is highlighted. When the point is after the closing character of one of the paired characters, the other is highlighted.
- **le-which-key**: The built-in which-key package dynamically displays available keybindings in a popup or dedicated buffer as a key sequence is entered. It facilitates discovery and retention of key combinations by presenting context-sensitive completions, thereby enhancing navigation through complex or highly customized keymaps.

These are core modules that are always enabled:
- **le-default-settings**: Configure enhanced default settings, including improved defaults, Tree-sitter language source URLs for 54 programming languages (`treesit-language-source-alist`), backup files, warnings to ignore, a minibuffer depth indicator, scrolling, window behavior... *This is one of the most important modules. Keep it enabled.*
- **le-theme**: The `le-theme` module loads the default theme. It can be configured via the `lightemacs-theme-name` variable. To customize this theme, modify the variable in your `~/.emacs.d/lisp/local/config.el` as follows:

## Modules Disabled by Default

### Disabled by default: le-group-evil (Vim Keybindings)

By default, [evil-mode](https://github.com/emacs-evil/evil) is disabled.

To enable it, add the following to the `~/.emacs.d/lisp/local/config.el` file:
```emacs-lisp
;; Enable Vim key bindings
(add-to-list 'lightemacs-modules 'le-group-evil)
```

Here are a few interesting features that Lightemacs provides:
- Pressing `-` opens a dired buffer for the directory containing the current file, automatically selecting that file. This provides a fast way to navigate and manage files without manually switching to the directory.

The `le-group-evil` group of modules includes:
- **le-evil** and *le-evil-collection**: Vim keybindings (evil and evil-collection). This module also extends Evil with several improvements: it resolves an Eldoc issue where help text would persist after deleting with Evil, synchronizes `evil-shift-width` with `tab-width`, provides refined Evil defaults, enables opening a Dired buffer for the current file’s directory with the file preselected by pressing the `-` key, and includes additional enhancements.
- **le-evil-commentary**: Comment or uncomment text in Normal or Visual mode by pressing `gc`.
- **le-evil-surround**: Enables text surrounding in visual state using `S<textobject>` or `gS<textobject>`. For example, selecting text and pressing `S"` will wrap it in double quotes.
- **le-goto-chg**: Navigate to the most recent edit in the buffer using `goto-last-change` or `goto-last-change-reverse`. Commonly used in `evil-mode` for the motions `g;` and `g,`, as well as for the last-change register `.`.

### Disabled by default: Automatically Remove Trailing Whitespace before Saving a Prog-mode Buffer

The **le-stripspace** module configures the [stripspace](https://github.com/jamescherti/stripspace.el) Emacs package, which automatically removes trailing whitespace and blank lines at the end of the buffer when saving.

(Trailing whitespace refers to any spaces or tabs that appear at the end of a line, beyond the last non-whitespace character. These characters serve no purpose in the content of the file and can cause issues with version control, formatting, or code consistency. Removing trailing whitespace helps maintain clean, readable files.)

It also includes an optional feature (`stripspace-only-if-initially-clean`, disabled by default), which, when enabled, ensures that trailing whitespace is removed only if the buffer was initially clean. This prevents unintended modifications to buffers that already contain changes, making it useful for preserving intentional whitespace or avoiding unnecessary edits in files managed by version control.

By default, `stripspace-local-mode` is enabled in `prog-mode`, `conf-mode`, and `text-mode`.

Here are some customizations for `stripspace-local-mode`:
```emacs-lisp
;; The `stripspace-only-if-initially-clean' option:
;; - nil to always delete trailing whitespace.
;; - Non-nil to only delete whitespace when the buffer is clean initially. (The
;; initial cleanliness check is performed when `stripspace-local-mode' is
;; enabled.)
(setq stripspace-only-if-initially-clean nil)

;; Enabling `stripspace-restore-column' preserves the cursor's column position
;; even after stripping spaces. This is useful in scenarios where you add extra
;; spaces and then save the file. Although the spaces are removed in the saved
;; file, the cursor remains in the same position, ensuring a consistent editing
;; experience without affecting cursor placement.
(setq stripspace-restore-column t)
```

### Disabled by default: le-treesit-auto (better syntax highlighting)

Module: **le-treesit-auto**

The **le-treesit-auto** module automatically installs and enables Tree-sitter major modes in Emacs 29 and later. If the Tree-sitter parser is unavailable or incompatible, it falls back to the original major mode. Tree-sitter is an incremental parsing system introduced in Emacs 29 that delivers precise, high-performance syntax highlighting. It supports a wide range of programming languages, including Bash, C, C++, C#, CMake, CSS, Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML, Elisp, Lua, and many others.

To enable it, add the following to the `~/.emacs.d/lisp/local/config.el` file:
```emacs-lisp
;; Tree-sitter is an incremental parsing system introduced in Emacs 29 that
;; provides precise, high-performance syntax highlighting. It supports a broad
;; set of programming languages, including Bash, C, C++, C#, CMake, CSS,
;; Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML,
;; Elisp, Lua, and many others. treesit-auto
(add-to-list 'lightemacs-modules 'le-treesit-auto)
```

### Disabled by default: Runs code formatters asynchronously (le-apheleia)

The **le-apheleia** module configures, [Apheleia](https://github.com/radian-software/apheleia), a package that runs code formatters asynchronously without disrupting the cursor position. Code formatters like Shfmt, Black and Prettier ensure consistency and improve collaboration by automating formatting, but running them on save can introduce latency (e.g., Black takes around 200ms on an empty file) and unpredictably move the cursor when modifying nearby text.

Apheleia solves both problems across all languages, replacing language-specific packages like Blacken and prettier-js. It does this by invoking formatters in an `after-save-hook`, ensuring changes are applied only if the buffer remains unmodified.

To maintain cursor stability, Apheleia generates an RCS patch, applies it selectively, and employs a dynamic programming algorithm to reposition the cursor if necessary. If the formatting alters the vertical position of the cursor in the window, Apheleia adjusts the scroll position to preserve visual continuity across all displayed instances of the buffer. This allows enjoying automated code formatting without sacrificing editor responsiveness or usability.

The **mod-apheleia** loads *apheleia* in a deferred manner and remains inactive until explicitly enabled, which helps minimize startup time and resource usage.

Here is an example you could place in `~/.emacs.d/lisp/local/config.el` to configure Apheleia for Bash/sh, Python, and Emacs Lisp:
```emacs-lisp
;; By default, `lightemacs-apheleia-mode-target-hooks' is set to `prog-mode-hook'.
(setq lightemacs-apheleia-target-hooks '(;; Python
                                         python-mode-hook
                                         python-ts-mode-hook
                                         ;; Bash/sh
                                         sh-mode-hook
                                         bash-ts-mode-hook
                                         ;; Elisp
                                         emacs-lisp-mode-hook))
```

### Disabled by default: Persisting and Restoring all buffers, windows/split, tab-bar, frames... (le-easysession)

The **le-easysession** module configures [easysession](https://github.com/jamescherti/easysession.el), a session manager for Emacs that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, windows/splits, the built-in tab-bar (including tabs, their buffers, and windows), and Emacs frames. It offers a convenient and effortless way to manage Emacs editing sessions and utilizes built-in Emacs functions to persist and restore frames.

With **easysession**, your Emacs setup is restored automatically when you restart. All files, Dired buffers, and window layouts come back as they were, so you can continue working right where you left off. While editing, you can also switch to another session, switch back, rename sessions, or delete them, giving you full control over multiple work environments.

To enable the module, add the following to `~/.emacs.d/lisp/local/config.el`:
```emacs-lisp
;; Enable the `le-easysession' module
(add-to-list 'lightemacs-modules 'le-easysession)
```

Usage:

- `M-x easysession-switch-to` to switch to another session or `easysession-load` to reload the current one,
- `M-x easysession-save-as` to save the current session as the current name or another name.

The following key bindings are defined for working with **EasySession**. All commands are grouped under the prefix `C-c s` for consistency and ease of recall:

- **`C-c ss`** → Save the current session under a new name (`easysession-save-as`).
- **`C-c sl`** → Switch to an existing session (`easysession-switch-to`).
- **`C-c sr`** → Rename the current session (`easysession-rename`).
- **`C-c sL`** → Load a previously saved session (`easysession-load`).
- **`C-c sS`** → Save the current session (`easysession-save`).

The **le-easysession** module automatically persists and restores the *scratch* buffer. This behavior is enabled by default, but it can be disabled by setting the variable `lightemacs-easysession-save-scratch` to nil.

Customizations:

* **`lightemacs-easysession-load-session-on-startup`** (Default: `t`): If non-nil, Emacs will automatically restore the main session on startup. Set this to `nil` to disable automatic session loading.

* **`lightemacs-easysession-restore-geometry-on-startup`** (Default: `t`): If non-nil, window geometry (size and position of frames) is restored along with the session. This setting works together with `lightemacs-easysession-load-session-on-startup`. Set to `nil` to ignore window size and position during session restoration.

### Disabled by default: Efficient template expansion with snippets (le-yasnippet and le-yasnippet-snippets)

The **le-yasnippet** configures [yasnippet](https://github.com/joaotavora/yasnippet), a package that provides a template system that enhances text editing by enabling users to define and use snippets, which are predefined templates of code or text. The user triggers snippet expansion by pressing the Tab key after typing an abbreviation, such as `if`. Upon pressing Tab, YASnippet replaces the abbreviation with the corresponding full template, allowing the user to fill in placeholders or fields within the expanded snippet.

The **le-yasnippet-snippets** configures the [yasnippet-snippets](https://github.com/AndreaCrotti/yasnippet-snippets) package, which provides a comprehensive collection of bundled templates for numerous programming and markup languages, including C, C++, C#, Perl, Python, Ruby, SQL, LaTeX, HTML, CSS...

### Disabled by default: An alternative terminal (le-vterm)

The le-vterm configures [vterm](https://github.com/akermu/emacs-libvterm) is an Emacs terminal emulator that provides a fully interactive shell experience within Emacs, supporting features such as color, cursor movement, and advanced terminal capabilities.

Unlike simpler Emacs terminal modes, `vterm` leverages the underlying libvterm C library for high-performance, accurate terminal emulation, allowing users to run shell programs, text-based applications, and REPLs seamlessly.

**You can launch `vterm` using:** `M-x vterm`

**Requirements:**

Before installing emacs-libvterm, you need to make sure you have installed
 1. GNU Emacs (>= 25.1) with [module support](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html). You can check that, by verifying that `module-file-suffix` is not `nil`.
 2. cmake (>= 3.11)
 3. libtool-bin (related issues: [#66](https://github.com/akermu/emacs-libvterm/issues/66) [#85](https://github.com/akermu/emacs-libvterm/issues/85#issuecomment-491845136))
 4. OPTIONAL: [libvterm](https://github.com/Sbozzolo/libvterm-mirror.git) (>= 0.2). This library can be found in the official repositories of most distributions (e.g., Arch, Debian, Fedora, Gentoo, openSUSE, Ubuntu). Typical names are `libvterm` (Arch, Fedora, Gentoo, openSUSE), or `libvterm-dev` (Debian, Ubuntu). If not available, `libvterm` will be downloaded during the compilation process. Some distributions (e.g. Ubuntu < 20.04, Debian < 11) have versions of `libvterm` that are too old. If you find compilation errors related to `VTERM_COLOR`, you should not use your system libvterm. See [FAQ](#frequently-asked-questions-and-problems) for more details.

To enable **le-vterm**, add the following to `~/.emacs.d/lisp/local/config.el`:
```emacs-lisp
;; Enable the `le-vterm' module
(add-to-list 'lightemacs-modules 'le-vterm)
```

### Disabled by default: Better Elisp editing (le-group-emacs-lisp)

The **le-group-emacs-lisp** group enables the following modules:
- **le-highlight-defined**: Configures [highlight-defined](https://github.com/Fanael/highlight-defined), a minor mode that highlights defined Emacs Lisp symbols. To enable the module, add the following to `~/.emacs.d/lisp/local/config.el`:
  ```emacs-lisp
  ;; Enable the `le-easy-escape' module
  (add-to-list 'lightemacs-modules 'highlight-defined)
  ```
- **le-page-break-lines**: Configures [page-break-lines-mode](https://github.com/purcell/page-break-lines), a minor mode that visually replaces ASCII form-feed characters (typically `^L`) with horizontal lines to make page breaks easier to see, without altering the underlying text. To enable the module, add the following to `~/.emacs.d/lisp/local/config.el`:
  ```emacs-lisp
  ;; Enable the `le-page-break-lines' module
  (add-to-list 'lightemacs-modules 'le-page-break-lines)
  ```
  (The **le-page-break-lines** module enables `page-break-lines-mode` whenever `emacs-lisp-mode-hook` is triggered.)

- **le-aggressive-indent**: Configures [aggressive-indent](https://github.com/Malabarba/aggressive-indent-mode), a minor mode that ensures Elisp code remains consistently indented. It automatically reindents after every modification, providing greater reliability than `electric-indent-mode`. To enable the module, add the following to `emacs-lisp-mode-hook` by adding the following to `~/.emacs.d/lisp/local/config.el`:
  ```emacs-lisp
  ;; Enable the `le-aggressive-indent' module
  (add-to-list 'lightemacs-modules 'le-aggressive-indent)
  ```
  (The **le-aggressive-indent** module enables `aggressive-indent-mode` whenever `emacs-lisp-mode-hook` or `scheme-mode-hook` are triggered.)

- **le-easy-escape**: Configures [easy-escape](https://github.com/cpitclaudel/easy-escape) improves the readability of Emacs Lisp regular expressions through syntax highlighting and character composition. Specifically, it hides double backslashes before regexp special characters `()|`, renders other doubled backslashes as single ones, and highlights them with a distinct face. These transformations affect only the visual presentation; the underlying buffer text remains unchanged. To enable the module, add the following to `~/.emacs.d/lisp/local/config.el`:
  ```emacs-lisp
  ;; Enable the `le-easy-escape' module
  (add-to-list 'lightemacs-modules 'le-easy-escape)
  ```
  (The **le-easy-escape** module enables `easy-escape-minor-mode` whenever `emacs-lisp-mode-hook` is triggered.)

- **le-elisp-refs**: Configures [elisp-refs](https://github.com/Wilfred/elisp-refs), an advanced code search for Emacs Lisp. It identifies references to functions, macros, variables, specials, and symbols by parsing the code instead of relying on plain text search. This guarantees precise results, eliminating false matches from comments or from identifiers that merely share the same name. The following commands are available: `elisp-refs-function`, `elisp-refs-macro`, `elisp-refs-variable`, `elisp-refs-special`, and `elisp-refs-symbol`. To enable the module, add the following to `~/.emacs.d/lisp/local/config.el`:
  ```emacs-lisp
  ;; Enable the `le-elisp-refs' module
  (add-to-list 'lightemacs-modules 'le-elisp-refs)
  ```

### Disabled by default: Indentation bars (le-indent-bars)

The **le-indent-bars** configures the [indent-bars](https://github.com/jdtsmith/indent-bars) packages, which enhances code readability by providing visual indentation guides, optimized for speed and customization. (Useful for Yaml or Python files.)

<img width="600" src="https://github.com/jdtsmith/indent-bars/assets/93749/0eaa0d85-0893-4893-8a56-a63ab6eeac1c"/>

It supports both space and tab-based indentation and offers optional tree-sitter integration, which includes features like scope focus. The appearance of the guide bars is highly customizable, allowing you to adjust their color, blending, width, position, and even apply a zigzag pattern.

It can be enabled interactively with `M-x indent-bars-mode` or set to load automatically. For instance, add the following to your `~/.emacs.d/lisp/local/config.el` to enable it for Python and YAML files:

```emacs-lisp
;; Enable the `le-indent-bars' module
(add-to-list 'lightemacs-modules 'le-indent-bars)

;; Enable indent-bars-mode automatically for Python files and Yaml files
(add-hook 'yaml-ts-mode-hook #'indent-bars-mode)
(add-hook 'yaml-mode-hook #'indent-bars-mode)
(add-hook 'python-mode-hook #'indent-bars-mode)
(add-hook 'python-ts-mode-hook #'indent-bars-mode)
```

(By default, Lightemacs sets `indent-bars-prefer-character` to `t` because it is more reliable and compatible with a wider range of configurations. If [stipples](https://github.com/jdtsmith/indent-bars?tab=readme-ov-file#stipples) render correctly on your system, you can set `indent-bars-prefer-character` to `nil`.)

### Enabled by Default: Expand Region (le-expand-region)

The **le-expand-region** module configures the [expand-region](https://github.com/magnars/expand-region.el) package, which allows you to progressively enlarge your text selection.

Pressing `C-=` (`Control` + `=`) initially selects a small unit, such as a word. Subsequent presses expand the selection to increasingly larger syntactic units—first the containing sentence, then the paragraph, and potentially the entire function.

Continue pressing `C-=` until the selection encompasses exactly the text you want.

### Other modules disabled by default

Here are a few other modules disabled by default:

-- **le-treesit-fold**: Configures [treesit-fold](https://github.com/emacs-tree-sitter/treesit-fold), which provides intelligent code folding by leveraging the structural understanding of the built-in tree-sitter parser (available in Emacs 29+). Unlike traditional folding methods that rely on regular expressions or indentation, treesit-fold uses the actual syntax tree of the code to accurately identify foldable regions such as functions, classes, comments, and documentation strings. This allows for faster and more precise folding behavior that respects the grammar of the programming language, ensuring that fold boundaries are always syntactically correct even in complex or nested code structures. By default, the module does not start `treesit-fold-mode` automatically. To enable it in specific modes such as `python-ts-mode`:
  ```emacs-lisp
  (add-hook 'python-ts-mode-hook #'treesit-fold-mode)
  ```

- **le-rainbow-delimiters**: Configures [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) is a minor mode that highlights parentheses, brackets, and braces according to their nesting depth, with each level displayed in a distinct color. This makes it easier to identify matching delimiters, navigate code structure, and understand which statements are at a given depth. Here is an example you could place in `~/.emacs.d/lisp/local/config.el` to configure rainbow-delimiters for Emacs Lisp:
  ```emacs-lisp
  ;; By default, `lightemacs-rainbow-delimiters-target-hooks' is set to '(emacs-lisp-mode-hook)
  (setq lightemacs-rainbow-delimiters-target-hooks '(emacs-lisp-mode-hook))
  ```


- **le-dumb-jump**: Configures [Dumb-jump](https://github.com/jacktasia/dumb-jump), a context-aware go to definition functionality for 50+ programming languages without requiring a language server. It works by using simple heuristics and regular expression searches to locate the definitions of functions, variables, and symbols across project files. Unlike more sophisticated language-aware tools, `dumb-jump` does not parse code semantically, which makes it lightweight and fast, but sometimes less precise (For greater precision, install a language server and enable Eglot; it will replace dumb-jump in the buffers where it is active.). It integrates with popular navigation packages like `xref`, allowing implementations with minimal configuration. users to jump to definitions or references.

- **le-avy**: Configures [Avy](https://github.com/abo-abo/avy), an Emacs package that provides a fast and efficient method for navigating to visible text in a buffer by jumping directly to characters, words, or lines. It allows the user to type a sequence of characters or select from highlighted targets to move the cursor instantly, reducing the need for repetitive cursor motions or scrolling. `C-:` is set to `avy-goto-char`, allowing the cursor to jump directly to any single character visible in the buffer. `C-'` is bound to `avy-goto-char-2`, enabling jumps to a specific sequence of two characters for more precise targeting. `M-g j` is assigned to `avy-goto-char-timer`, which interactively highlights characters and lets the user type keys over time to select a target, useful for dynamic or ongoing navigation. Finally, `M-g w` is bound to `avy-goto-word-1`, allowing rapid jumps to the first character of any visible word. Together, these bindings provide a flexible, keyboard-driven system for efficiently moving around text.

- **le-gcmh**: Gcmh (Garbage Collector Magic Hack) optimizes Emacs’ garbage collection behavior by adjusting the garbage collection threshold dynamically. Instead of collecting memory frequently during normal editing, gcmh increases the threshold while Emacs is idle, reducing interruptions and improving perceived performance. It also restores the threshold during active usage to prevent excessive memory use. In essence, it makes Emacs feel more responsive by tuning garbage collection automatically.

- **le-magit**: Configures [Magit](https://github.com/magit/magit/) provides a comprehensive interface to the Git version control system. It aims to serve as a full-featured Git porcelain. Although it does not yet cover every Git command, it is sufficiently complete to enable even experienced Git users to perform nearly all their routine version control tasks entirely within Emacs. **Usage:** Press `C-x g`.

- **le-csv-mode**: Configures [csv-mode](https://elpa.gnu.org/packages/csv-mode.html), which provides a major mode that transforms the experience of editing Comma-Separated Value files by bridging the gap between raw text manipulation and a spreadsheet interface. Its most impactful feature is the ability to align fields into visually distinct, vertically synchronized columns, which instantly renders dense, delimited text into a readable table format. Beyond mere visualization, the package provides tools for structural editing, allowing users to sort rows based on specific fields, transpose columns, and perform rectangular operations such as killing or yanking specific vertical slices of data.

- **git-modes**: Emacs major modes for managing Git configuration files, such as `.gitattributes`, `.gitconfig`, and `.gitignore`.

- **le-diff-hl**: Configures the *diff-hl* package, which highlights uncommitted changes in the window margin, enabling navigation between them. Also known as source control gutter indicators, it displays added, modified, and deleted lines in real time. In Git-controlled buffers, changes can be staged and unstaged directly, providing a clear view of version-control changes without running `git diff`. By default, the module does not start `diff-hl-mode` automatically. To enable it in specific modes:
  ```emacs-lisp
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  ```

- **le-display-line-numbers**: Enables the built-in `display-line-numbers-mode` line numbers in the buffer's display, showing the current line number next to each line. It updates dynamically as lines are added, removed, or scroll lines, but they don’t change the actual text.

- **le-ace-window**: Configures [ace-window](https://github.com/abo-abo/ace-window) provides a fast and efficient method for switching between windows in a frame. Instead of cycling through windows sequentially or using more cumbersome key sequences, Ace Window displays a single-letter label on each visible window, allowing the user to jump directly to a target window by pressing the corresponding key. The `other-window` keybinding is remapped to `ace-window`, which provides a faster and more visual method for switching between windows (default `C-x o`).

- **le-helpful**: Configures [Helpful](https://github.com/Wilfred/helpful), an enhanced alternative to the built-in help system that provides richer, context-aware information about symbols, functions, variables, and macros. In contrast to the default describe-* commands, Helpful presents a unified, navigable buffer that integrates documentation strings, source code, keybindings, references, and even interactive examples, thereby offering a more comprehensive and efficient environment for exploring Emacs internals. To enable the module, add the following to `~/.emacs.d/lisp/local/config.el`:
  ```emacs-lisp
  ;; Enable the `le-helpful' module
  (add-to-list 'lightemacs-modules 'le-helpful)
  ```

- **le-compile-angel**: Configures [Compile-angel](https://github.com/jamescherti/compile-angel.el/), a package that speeds up Emacs by ensuring that all Elisp libraries are both byte-compiled and native-compiled. To enable the module, add the following to `~/.emacs.d/lisp/local/config.el`:
  ```emacs-lisp
  ;; Enable the `le-compile-angel' module by pushing it above other modules
  (add-to-list 'lightemacs-modules 'le-compile-angel)
  ```

- **le-eat**: Eat (Emulate A Terminal) is a terminal emulator implemented entirely in Emacs Lisp. It supports full-screen terminal applications such as Emacs itself and offers high performance, operating over three times faster than Term. Eat provides advanced features like Sixel graphics, complete mouse support, shell integration, and reduced screen flicker for smoother performance.

Elisp file-type modules are disabled by default:

- **le-group-yaml**: Configures [yaml-mode](https://github.com/yoshiki/yaml-mode) when Tree-sitter’s `yaml-ts-mode` is unavailable. (The variable `lightemacs-yaml-mode-prefer-treesitter` defaults to `t`, indicating a preference for using Tree-sitter for YAML editing whenever possible. Setting this variable to `nil` forces `yaml-mode` to load even if Tree-sitter is available.) It also ensures that the indentation adheres to the YAML standard: two spaces and no tabs.
  ```emacs-lisp
  ;; Enable the `le-paredit' module
  (add-to-list 'lightemacs-modules 'le-group-yaml)
  ```

- **le-paredit**: Configures [Paredit](https://paredit.org/), a package that assists in editing Lisp code by enforcing the structural integrity of s-expressions. Instead of treating parentheses as ordinary characters, Paredit ensures that every edit operation, such as inserting, deleting, or moving expressions, preserves balanced parentheses and valid Lisp syntax. It provides structured editing commands for navigating, wrapping, splicing, or reformatting code, making it significantly easier to manipulate nested expressions without introducing syntactic errors. To enable the module, add the following to `~/.emacs.d/lisp/local/config.el`:
  ```emacs-lisp
  ;; Enable the `le-paredit' module
  (add-to-list 'lightemacs-modules 'le-paredit)
  ```
  (The **le-paredit** module activates `paredit-mode` when any of the following hooks is triggered: `emacs-lisp-mode-hook`, `lisp-interaction-mode-hook`, `ielm-mode-hook`, `lisp-mode-hook`, `eval-expression-minibuffer-setup-hook`, `cider-repl-mode-hook`, `clojure-mode-hook`, `geiser-repl-mode-hook`, `racket-mode-hook`, `racket-repl-mode-hook`, `scheme-mode-hook`, or `slime-repl-mode-hook`.)

- **le-enhanced-evil-paredit**: (Only for Evil and Paredit users) This module configures the [enhanced-evil-paredit](https://github.com/jamescherti/enhanced-evil-paredit.el) package, which prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking any operation that would break the parenthetical structure. This ensures Lisp code remains syntactically correct while retaining the editing capabilities of *evil-mode*. This module automatically enables `enhanced-evil-paredit-mode` whenever `paredit-mode` is activated.

- **le-evil-snipe**: Provides two-character motions for rapid navigation within text, similar to Evil’s built-in `f`/`F`/`t`/`T` commands, but with incremental highlighting of potential targets as you type. By default, `s` (forward) and `S` (backward) are bound to `evil-snipe-s` and `evil-snipe-S`, respectively. **Usage:** Pressing `s` in normal mode prompts you to type two characters, then jumps the cursor to the nearest matching occurrence while highlighting all matches incrementally.

- **le-inhibit-mouse**: Enables the [inhibit-mouse](https://github.com/jamescherti/inhibit-mouse.el) package that disables of mouse input.

- **le-diminish**: Diminish reduces clutter in the mode line by hiding or shortening the names of minor modes you rarely need to see. This makes the interface cleaner and allows you to focus only on the information that is actually useful.

* **le-xclip**: This module integrates the [xclip](https://elpa.gnu.org/packages/xclip.html) package, enabling copy and paste between Emacs running in a terminal and the system GUI clipboard. (IMPORTANT: The le-xclip package only activates the xclip package when Emacs is executed in a terminal.) The xclip package relies on external command-line tools depending on the platform: `xclip` or `xsel` for X11, `pbpaste`/`pbcopy` for macOS, `getclip`/`putclip` for Cygwin, `wl-clipboard` for Wayland, and `termux-clipboard-get`/`termux-clipboard-set` for Termux.

- **le-shut-up**: The *shut-up* package suppresses output from functions that normally print to the *Messages* buffer or to the echo area. It provides a macro called `shut-up` that temporarily silences messages while evaluating its body. This is useful when running code that would otherwise clutter the user's *Messages* buffer with unnecessary output.

- **le-evil-visualstar**: Enables [evil-visualstar](https://github.com/bling/evil-visualstar), which allows searching for the current visual selection using `*` or `#`. **Usage:** Create a visual selection with `v` or `V`, then press `*` to search forward or `#` to search backward. When `evil-visualstar/persistent` is non-nil, visual state remains active, enabling repeated searches without reselecting the text.

### Interesting builtin modules that Lightemacs configures

Here are the built-in packages that Lightemacs enhances the defaults:

- `M-x proced`: A built-in Emacs utility that provides a specialized interface for monitoring and managing system processes, effectively acting as an interactive version of the Unix `ps` or `top` commands.

## Other Features

In addition to modules, Lightemacs provides the following features:
- Reduced clutter: A `var/` directory (e.g., `~/.emacs.d/var/`) is used to store all files that Emacs normally places in the base directory (e.g., `~/.emacs.d`). By default, Emacs stores configuration files, caches, backups, and other data within `~/.emacs.d`, which can accumulate over time and complicate management.
- Lightemacs configures `use-package` to automatically refresh Emacs package archives once per session when installing a package that isn’t already present. This allows missing packages to be installed without manually running `package-refresh-contents` and prevents repeated refreshes in the same session, resolving issues such as invalid GPG keys when installing packages after the package list has become outdated.

## Useful variables, functions, and macros

### Useful variables

#### Ellipsis
Change the default Ellipsis using the `lightemacs-ellipsis` variable, which defaults to `" ▼"`. This string used to indicate folded sections in `org-mode`, `outline-mode`, `outline-minor-mode`... This ellipsis appears at the end of a heading or section that has been collapsed. Modify the variable in your `~/.emacs.d/lisp/local/config.el` as follows:
```emacs-lisp
(setq lightemacs-ellipsis " ▼")
```

#### Cycling

The `lightemacs-cycle` variable controls whether cycling through completion candidates is enabled.

- If non-nil (default), navigating past the last candidate wraps around to the first, and vice versa. This applies to Vertico minibuffer completions, Corfu code completions, and Evil search candidates.
- If nil, selection stops at the first or last candidate without wrapping.

To disable cycling (default: enabled), add the following to your `~/.emacs.d/lisp/local/config.el`:

```emacs-lisp
;; Disable cycling
;;
;; Navigating past the last candidate stops at the first or last candidate
;; without wrapping. This applies to Vertico minibuffer completions, Corfu code
;; completions, and Evil search candidates.
(setq lightemacs-cycle nil)
```

To enable cycling (default: enabled), add the following to your `~/.emacs.d/lisp/local/config.el`:

```emacs-lisp
;; Enable cycling
;;
;; Navigating past the last candidate wraps around to the first, and vice versa.
;; This applies to Vertico minibuffer completions, Corfu code completions, and
;; Evil search candidates.
(setq lightemacs-cycle t)
```

#### Other variables

- `lightemacs-native-comp-excluded-cpus` (default: `3`): By default, Emacs uses only half of the available CPUs for native compilation. The `lightemacs-native-comp-excluded-cpus` variable adjusts that behavior by reserving the specified number of CPUs and using the remainder for native compilation, thereby increasing parallelism and speeding up the process. Set this to `nil` to disable CPU reservation entirely.

- `lightemacs-excluded-packages`: List of package symbols that should be excluded from initialization. Each element must be a symbol naming a package that would otherwise be initialized by Lightemacs. Packages listed here are skipped during the initialization process. Only packages declared via `lightemacs-module` are affected by this variable.

- `lightemacs-verbose`: Enable displaying verbose messages in the `*Messages*` buffer.

- `lightemacs-module-refresh-contents`: If non-nil, `lightemacs-module` may refresh package contents once. Refresh package contents when `lightemacs-module-refresh-contents` is non-nil and the package is not installed.

- `lightemacs-package-manager`: Specifies which package manager to use in Lightemacs. Choices are: `'use-package`, `'straight`, or `'elpaca`. This variable controls how `lightemacs-module` handles installation and configuration of packages.

- `lightemacs-load-compiled-init-files`: If non-nil, attempt to load byte-compiled .elc for init files. This will enable Lightemacs to load byte-compiled or possibly native-compiled init files for the following initialization files: init.el, pre-init.el, post-init.el, pre-early-init.el, and post-early-init.el.

### Useful functions and macros

- `lightemacs-recenter-if-out-of-view` (macro): Execute BODY and recenter if point is outside the original window bounds.
- `lightemacs-save-window-hscroll` (macro): Execute BODY while preserving the horizontal scroll of the selected window. This macro saves the current `window-hscroll` of the selected window. After BODY executes, the horizontal scroll is restored exactly, leaving the vertical position and window start unchanged. Use this macro when you only need to maintain horizontal alignment, without restoring the lines above the cursor.
- `lightemacs-save-window-start` (macro): Preserve and restore `window-start` relative to the lines above the cursor. This macro saves the first visible line in the selected window. After BODY executes, the window is restored so that the same lines remain visible above the cursor, maintaining the relative vertical position of the cursor within the window. To also restore the mark, this macro can be combined with `save-mark-and-excursion`. For preservation of horizontal scroll only (hscroll), consider using the `lightemacs-save-window-hscroll` macro.

## Author and license

The *Lightemacs* project has been written by James Cherti and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2025-2026 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program (in the .LICENSE file).

## Links

- [Lightemacs @GitHub](https://github.com/jamescherti/lightemacs)

Other Emacs projects by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): The Lightemacs project is based on the minimal-emacs.d, an optimized Emacs base (init.el and early-init.el) that gives you full control over your configuration. It provides better defaults, an optimized startup, and a clean foundation for building your own vanilla Emacs setup. Building the minimal-emacs.d init.el and early-init.el was the result of extensive research and testing to fine-tune the best parameters and optimizations for an Emacs configuration.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim's Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
- [kirigami.el](https://github.com/jamescherti/kirigami.el): The *kirigami* Emacs package offers a unified interface for opening and closing folds across a diverse set of major and minor modes in Emacs, including `outline-mode`, `outline-minor-mode`, `outline-indent-minor-mode`, `org-mode`, `markdown-mode`, `vdiff-mode`, `vdiff-3way-mode`, `hs-minor-mode`, `hide-ifdef-mode`, `origami-mode`, `yafolding-mode`, `folding-mode`, and `treesit-fold-mode`. With Kirigami, folding key bindings only need to be configured **once**. After that, the same keys work consistently across all supported major and minor modes, providing a unified and predictable folding experience.
