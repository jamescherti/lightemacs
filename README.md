# Lightemacs - Fast and Lightweight Emacs framework

**Author:** James Cherti

**Introduction:**

The Lightemacs project is a **fast and lightweight Emacs framework** that uses modern Emacs features and essential packages, which enhance Emacs by improving completion, navigation, editing efficiency, and overall usability.

Lightemacs provides a range of modules that can be selectively enabled or disabled according to your preferences, with all modules ensuring packages are loaded only when needed, **enabling exceptionally fast, deferred startup**.

![](https://www.jamescherti.com/misc/screenshot-minimal-emacs-3.png)

Here are some of the modules that are enabled by default:

- Better sorting and filtering `M-x` (Vertico) and completion (Corfu) candidate lists with prescient.el, which adaptively ranks candidates based on selection frequency and recency, ensuring commonly used options or completions appear first.
- Ensure that all Elisp libraries are both byte-compiled and native-compiled to speed up Emacs.
- Ensure that all adjustments made with `text-scale-increase` and `text-scale-decrease` are persisted and restored across sessions
- Improved undo/redo functionality with persistent undo history saved and restored across Emacs sessions, even after restarts.
- The Yasnippet template system that enhances text editing by enabling users to define and use snippets.
- Preserve minibuffer history between sessions (savehist), persist and restore cursor position (saveplace), automatically update buffer contents to reflect changes in the underlying file on disk (autorevert), and maintain a list of recently accessed files (recentf).
- Additional filetypes: `markdown-mode` and `org-mode`.
- Enhanced Emacs Lisp (Elisp) Editing Experience:
    - Highlights defined Emacs Lisp symbols.
    - Maintain consistent indentation of Elisp code during editing.
    - Visually replace ASCII form-feed characters (typically `^L`) with horizontal lines to make page breaks easier to see.
- Dired: Configure dired to group directories first and enable dired-filter to hide dotfiles, omit specified files, and exclude files listed in `.gitignore`.
- Change the default Ellipsis using the `lightemacs-ellipsis` variable, which defaults to `" ▼"` String used to indicate folded sections in `org-mode`, `outline-mode`, `outline-minor-mode`...
- Save and restore the default theme using the `lightemacs-default-theme` variable.
- Press `C-=` to expand the selection step by step, from a word to a sentence, paragraph, or entire function, until it covers the text you want.
- And more.

Optionally, you can enable the following features that are disabled by default:
- **group-evil**: Vim keybindings (Evil) with additional functionality, including commenting/uncommenting, two-character search using the `s` key (as an alternative to the `f` key), and surrounding text in visual state.
- **mod-treesit-auto**: Better Syntax highlighting with Tree-sitter. (If the Tree-sitter parser is unavailable or incompatible, it falls back to the original major mode.)

**What is the difference between Lightemacs and minimal-emacs.d?**

The Lightemacs project is built upon the [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) initialization files (`init.el` and `early-init.el`), allowing it to be configured identically to minimal-emacs.d while inheriting its high-quality default settings and efficient startup performance.

Unlike minimal-emacs.d, which provides a minimal and highly flexible Emacs configuration with only essential defaults, Lightemacs extends this foundation by enabling a curated set of modern features and optimizations out of the box. While minimal-emacs.d requires users to manually configure and enable most enhancements, Lightemacs activates performance improvements, advanced completion systems, persistent undo, snippet support, and additional filetype modes automatically, while still retaining full configurability and compatibility with minimal-emacs.d’s initialization files.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [Lightemacs - Fast and Lightweight Emacs framework](#lightemacs---fast-and-lightweight-emacs-framework)
  - [Requirements](#requirements)
  - [Install Lightemacs](#install-lightemacs)
    - [Install Lightemacs into ~/.emacs.d](#install-lightemacs-into-emacsd)
    - [Alternative: Install Lightemacs into ~/.lightemacs.d](#alternative-install-lightemacs-into-lightemacsd)
  - [Update Lightemacs](#update-lightemacs)
  - [The hierarchy of Lightemacs files](#the-hierarchy-of-lightemacs-files)
    - [Files that must not be modified:](#files-that-must-not-be-modified)
    - [Files you may edit:](#files-you-may-edit)
  - [Customizations](#customizations)
    - [Never modify init.el and early-init.el. Modify these instead...](#never-modify-initel-and-early-initel-modify-these-instead)
    - [How to enable the menu-bar, the tool-bar, dialogs, the contextual menu, and tooltips?](#how-to-enable-the-menu-bar-the-tool-bar-dialogs-the-contextual-menu-and-tooltips)
  - [Modules Enabled by Default](#modules-enabled-by-default)
    - [Default theme (mod-default-theme)](#default-theme-mod-default-theme)
    - [Better minibuffer and navigation (mod-consult, mod-embark, and mod-vertico, mod-marginalia)](#better-minibuffer-and-navigation-mod-consult-mod-embark-and-mod-vertico-mod-marginalia)
    - [Better completion (mod-corfu and mod-cape)](#better-completion-mod-corfu-and-mod-cape)
    - [Better sorting and ordering (mod-prescient, mod-corfu-prescient, and mod-vertico-prescient)](#better-sorting-and-ordering-mod-prescient-mod-corfu-prescient-and-mod-vertico-prescient)
    - [Better File Manager (mod-dired and mod-dired-filter)](#better-file-manager-mod-dired-and-mod-dired-filter)
    - [Efficient template expansion with snippets (mod-yasnippet and mod-yasnippet-snippets)](#efficient-template-expansion-with-snippets-mod-yasnippet-and-mod-yasnippet-snippets)
    - [Better undo/redo (mod-undo-fu and undo-fu-session)](#better-undoredo-mod-undo-fu-and-undo-fu-session)
    - [Keybindings (mod-keybindings)](#keybindings-mod-keybindings)
    - [Code folding based on indentation (mod-outline-indent)](#code-folding-based-on-indentation-mod-outline-indent)
    - [Save History (mod-savehist)](#save-history-mod-savehist)
    - [Save and Restore Cursor (mod-saveplace)](#save-and-restore-cursor-mod-saveplace)
    - [Auto Revert Buffer to Reflect Changes Made to the Underlying File on Disk (mod-autorevert)](#auto-revert-buffer-to-reflect-changes-made-to-the-underlying-file-on-disk-mod-autorevert)
    - [Persist and Restore Text Scale (mod-persist-text-scale)](#persist-and-restore-text-scale-mod-persist-text-scale)
    - [Automatically Remove Trailing Whitespace before Saving a Prog-mode Buffer](#automatically-remove-trailing-whitespace-before-saving-a-prog-mode-buffer)
    - [Expand Region (mod-expand-region)](#expand-region-mod-expand-region)
    - [A better way to rename or delete files (mod-buffer-file)](#a-better-way-to-rename-or-delete-files-mod-buffer-file)
    - [Recent files (mod-recentf)](#recent-files-mod-recentf)
    - [Detect indentation offset (mod-dtrt-indent)](#detect-indentation-offset-mod-dtrt-indent)
    - [Other Modules Enabled by Default](#other-modules-enabled-by-default)
    - [Enhanced Emacs Lisp (Elisp) Editing Experience (group-emacs-lisp)](#enhanced-emacs-lisp-elisp-editing-experience-group-emacs-lisp)
  - [Modules Disabled by Default](#modules-disabled-by-default)
    - [Disabled by default: mod-treesit-auto (better syntax highlighting)](#disabled-by-default-mod-treesit-auto-better-syntax-highlighting)
    - [Disabled by default: group-evil (Vim Keybindings)](#disabled-by-default-group-evil-vim-keybindings)
    - [Disabled by default: An alternative terminal (mod-vterm)](#disabled-by-default-an-alternative-terminal-mod-vterm)
    - [Disabled by default: Indentation bars (mod-indent-bars)](#disabled-by-default-indentation-bars-mod-indent-bars)
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
git clone https://github.com/jamescherti/lightemacs ~/.emacs.d
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
git -C ~/.emacs.d pull
```

## The hierarchy of Lightemacs files

If you install Lightemacs in `~/.emacs.d/`, the directory structure is as follows:

### Files that must not be modified:

- `~/.emacs.d/var/`: Contains all files generated dynamically by Emacs and its plugins. Lightemacs redirects such files here to prevent cluttering `~/.emacs.d/`.
- `~/.emacs.d/lisp/lightemacs/`: Contains the modules and libraries used by Lightemacs (do not modify these files).
- `~/.emacs.d/early-init.el` and `~/.emacs.d/init.el`: Initialization files (do not modify these files).

### Files you may edit:

- `~/.emacs.d/config.el`: The main configuration file, where you may adjust settings or install packages.
- `~/.emacs.d/lisp/local/`: A directory for your personal Elisp files.

*(Files and directories intended for user modification, such as `~/.emacs.d/config.el` or any files within `~/.emacs.d/lisp/local/`, are not tracked by Git.)*

## Customizations

### Never modify init.el and early-init.el. Modify these instead...

**The `init.el` and `early-init.el` files should never be modified directly** because they are intended to be managed by Git during an update.

Modify `~/.emacs.d/config.el` instead. This file is loaded after `init.el` but before the Lightemacs modules are initialized. It is intended for supplementary configurations or package setups.

Always begin your `config.el` file with the following header to prevent them from being byte-compiled and to activate lexical binding:
```elisp
;;; config.el --- Configuration -*- no-byte-compile: t; lexical-binding: t; -*-
```

*(Only if you know what you're doing: Removing `no-byte-compile: t;` from your init files allows Emacs to compile them, improving load and execution speed. However, if you do so, you may need to add required dependencies. For example, if you're using `use-package`, add `(require 'use-package)` at the top of `post-init.el` to ensure all necessary `use-package` variables and functions are loaded.)*

**Important:** The examples in this README reference pre/post init files in the `~/.emacs.d/` directory, but the `config.el` should be placed in the same directory as `init.el` and `early-init.el`, regardless of their location.

The Lightemacs project is based on the [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) initialization files, which means it can be configured in exactly the same way as minimal-emacs.d. (pre-init.el, post-init.el, pre-early-init.el, and post-early-init.el)

### How to enable the menu-bar, the tool-bar, dialogs, the contextual menu, and tooltips?

The Lightemacs project is based on the [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) initialization files, which means it can be configured in exactly the same way as minimal-emacs.d.

**Note:** Enabling the tool-bar or menu-bar may slightly increase your startup time.

To customize your Emacs setup to include various user interface elements, you can use the following settings in your ``~/.emacs.d/pre-early-init.el``:

``` emacs-lisp
(setq minimal-emacs-ui-features '(context-menu tool-bar menu-bar dialogs tooltips))
```

These settings control the visibility of dialogs, context menus, toolbars, menu bars, and tooltips.

## Modules Enabled by Default

### Default theme (mod-default-theme)

The `mod-default-theme` loads the default theme. It can be configured via the `lightemacs-default-theme` variable, which defaults to `"tomorrow-night-deepblue"`. To customize this theme, modify the variable in your `~/.emacs/config.el` as follows:

```emacs-lisp
(setq lightemacs-default-theme 'tomorrow-night-deepblue)
```

The default theme, Tomorrow Night Deepblue Emacs Theme, is a a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes:
![](https://www.jamescherti.com/misc/screenshot-minimal-emacs-3.png)

Here are examples of alternative built-in themes:
```elisp
(setq lightemacs-default-theme 'modus-operandi)
(setq lightemacs-default-theme 'modus-vivendi)
(setq lightemacs-default-theme 'tango)
(setq lightemacs-default-theme 'tango-dark)
```

### Better minibuffer and navigation (mod-consult, mod-embark, and mod-vertico, mod-marginalia)

Vertico, Consult, Marginalia, and Embark collectively enhance Emacs' completion and navigation capabilities:
- [Vertico](https://github.com/minad/vertico) provides a vertical completion interface, making it easier to navigate and select from completion candidates (e.g., when `M-x` is pressed).
- [Consult](https://github.com/minad/consult) offers a suite of commands for efficient searching, previewing, and interacting with buffers, file contents, and more, improving various tasks. *(Try `M-x consult-rg` to search your project using ripgrep, or `M-x consult-fd` to quickly locate files in your workspace.)*
- [Embark](https://github.com/oantolin/embark) integrates with these tools to provide context-sensitive actions and quick access to commands based on the current selection, further improving user efficiency and workflow within Emacs. Together, they create a cohesive and powerful environment for managing completions and interactions. *(While searching with `M-x consult-rg` or `M-x consult-fd`, you can use `M-x embark-export` to act on the search results. This allows you to export the matches to another buffer, perform batch operations, or open multiple files at once, providing an excellent way to manipulate and navigate large sets of results.)*
- [Marginalia](https://github.com/minad/marginalia) enriches minibuffer completions with contextual annotations. It Enhances Vertico by adding rich annotations to completion candidates, such as file sizes, documentation, or metadata.

![](https://github.com/minad/consult/blob/screenshots/consult-grep.gif?raw=true)

### Better completion (mod-corfu and mod-cape)

[Corfu](https://github.com/minad/corfu) enhances in-buffer completion by displaying a compact popup with current candidates, positioned either below or above the point. Candidates can be selected by navigating up or down.

[Cape](https://github.com/minad/cape), or Completion At Point Extensions, extends the capabilities of in-buffer completion. It integrates with Corfu or the default completion UI, by providing additional backends through completion-at-point-functions.

![](https://github.com/minad/corfu/blob/screenshots/popupinfo-dark.png?raw=true)

### Better sorting and ordering (mod-prescient, mod-corfu-prescient, and mod-vertico-prescient)

The **mod-prescient** configures [prescient.el](https://github.com/radian-software/prescient.el) is a library for sorting and filtering lists of candidates, such as those presented by packages like Vertico or Corfu.

The main benefit of *prescient.el* is that it adaptively orders candidates based on both frequency and recency of selection, making frequently used options appear first without sacrificing predictable filtering results.

- Vertico and Prescient (mod-vertico-prescient): When prescient.el is used with Vertico, prescient.el enhances minibuffer completion by dynamically reordering candidates based on frequency and recency, making it faster to select commonly used options while preserving consistent, predictable filtering. **Example:** When running `M-x` and repeatedly selecting the command `compile`, prescient.el will place `compile` near the top of the Vertico minibuffer list in future sessions, reducing the need to type its full name.
- Corfu and Prescient (mod-corfu-prescient): When prescient.el is used with Corfu, prescient.el improves both in-buffer completions and pop-up completion menus by making candidate ordering more predictable and adaptive to recent usage, thus speeding up repeated selections. **Example:** If you frequently choose the completion `printf` when editing C code, prescient.el will gradually move `printf` toward the top of the list whenever similar candidates are offered, reducing the number of keystrokes needed to select it.

### Better File Manager (mod-dired and mod-dired-filter)

Configure dired to group directories first and enable dired-filter to hide dotfiles, omit specified files, and exclude files listed in `.gitignore`.

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

The `mod-outline-indent` module can be enabled using `M-x outline-indent-minor-mode`.

The following example can be added to the `~/.emacs.d/config.el` file to automatically enable `outline-indent-minor-mode` for YAML and Python files:
```elisp
(add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
(add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)

(add-hook 'python-mode-hook #'outline-indent-minor-mode)
(add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)
```

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

To enable `stripspace-local-mode` for `prog-mode` (affecting all programming languages), add the following to the `~/.emacs.d/config.el` file:
```elisp
;; Enable it for `prog-mode-hook'
(add-hook 'prog-mode-hook #'stripspace-local-mode)
```

### Expand Region (mod-expand-region)

The **mod-expand-region** module configures the [expand-region](https://github.com/magnars/expand-region.el) package, which allows you to progressively enlarge your text selection.

Pressing `C-=` (`Control` + `=`) initially selects a small unit, such as a word. Subsequent presses expand the selection to increasingly larger syntactic units—first the containing sentence, then the paragraph, and potentially the entire function.

Continue pressing `C-=` until the selection encompasses exactly the text you want.

### A better way to rename or delete files (mod-buffer-file)

The **mod-buffer-file** configures [bufferfile](https://github.com/jamescherti/bufferfile.el), package that provides helper functions to delete, rename, or copy buffer files:
- `M-x bufferfile-rename`: Renames the file visited by the current buffer, ensures that the destination directory exists, and updates the buffer name for all associated buffers, including clones/indirect buffers. It also ensures that buffer-local features referencing the file, such as Eglot or dired buffers, are correctly updated to reflect the new file name.
- `M-x bufferfile-delete`: Delete the file associated with a buffer and kill all buffers visiting the file, including clones/indirect buffers.
- `M-x bufferfile-copy`: Ensures that the destination directory exists and copies the file visited by the current buffer to a new file.

The functions above also ensures that any modified buffers are saved prior to executing operations like renaming, deleting, or copying.

To replace the default *dired* rename command with `bufferfile-rename`, add the following to your `~/.emacs.d/config.el` file:

```elisp
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "R") #'bufferfile-dired-do-rename)

  ;; For users of `evil-mode', uncomment the following sexp:
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key 'normal dired-mode-map (kbd "R") #'bufferfile-dired-do-rename))
  )
```

To make *bufferfile* use version control when renaming or deleting files, add the following to your `~/.emacs.d/config.el` file:

```elisp
;; Use version control when renaming or deleting files with `bufferfile-rename'
(setq bufferfile-use-vc t)
```

### Recent files (mod-recentf)

Recentf maintains a list of recently accessed files, making it easier to reopen files you have worked on recently.

In addition to its built-in capabilities, the **mod-recentf** module provides the following enhancements:
- Inserts the current file at the beginning of the recent files list upon buffer switch.
- Cleans up the recent files list when quitting Emacs, prior to its automatic saving.
- Decrease recentf-mode verbosity by restricting its messages to the `*Messages*` buffer, preventing display in the minibuffer

### Detect indentation offset (mod-dtrt-indent)

The **mod-dtrt-indent** package configures the [dtrt-indent](https://github.com/jscheid/dtrt-indent) package, which provides functions to automatically detect the indentation offset, defined as the number of spaces or the tab width used for code indentation.

By default, the *dtrt-indent* package is loaded in a deferred manner and remains inactive until explicitly enabled, which helps minimize startup time and resource usage.

To use it, you have multiple options: you can activate `dtrt-indent-global-mode` to automatically detect and adjust the indentation offset in all buffers; alternatively, you can enable `dtrt-indent-mode` (local mode) to restrict the behavior to the current buffer; or you can invoke the function `dtrt-indent-adapt` directly whenever you need to adjust the indentation settings for a specific file or buffer.

For instance, configure it to run automatically in programming modes by adding the following to the `~/.emacs.d/config.el` file:

```emacs-lisp
;; Adjusting indentation settings for `prog-mode-hook` buffers.
(add-hook 'prog-mode-hook #'dtrt-indent-adapt 80)
```

(In this example, the function `dtrt-indent-adapt` is invoked for all buffers in `prog-mode` with a priority of 80, enabling Emacs to automatically detect and adjust to the file’s indentation style after the standard `prog-mode-hook` hooks.)

To prevent `dtrt-indent` from displaying a message each time it adjusts the indentation offset, silence these notifications by adding the following line to the `~/.emacs.d/config.el` file:

```emacs-lisp
(setq dtrt-indent-verbosity 0)
```

### Other Modules Enabled by Default

- **mod-outline**: Update the ellipsis in `outline-minor-mode` using the `lightemacs-ellipsis` variable. The `outline-minor-mode` enabled code folding in programming and can be configured by adding the following to the `~/.emacs.d/config.el` file:
  ```elisp
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  ```

  You can also, as an alternative to prog-mode-hook, add hook to specific modes:
  ```elisp
  (add-hook 'grep-mode-hook #'outline-minor-mode)
  (add-hook 'conf-mode-hook #'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'js-mode-hook #'outline-minor-mode)
  (add-hook 'js-ts-mode-hook #'outline-minor-mode)
  (add-hook 'markdown-ts-mode-hook #'outline-minor-mode)
  (add-hook 'markdown-mode-hook #'outline-minor-mode)
  ```
- **mod-compile-angel**: Compile-angel speeds up Emacs by ensuring that all Elisp libraries are both byte-compiled and native-compiled.
- **mod-vim-tab-bar**: Enhances Emacs’ built-in tab bar with a minimalist, Vim-inspired design that automatically adapts to the current Emacs theme.
- **mod-markdown-mode**: The [markdown-mode](https://github.com/jrblevin/markdown-mode) package provides a major mode for Emacs for syntax highlighting, editing commands, and preview support for Markdown documents. It supports core Markdown syntax as well as extensions like GitHub Flavored Markdown (GFM).
- **mod-org**: Configures Org mode and Org Agenda, a major mode designed for organizing notes, planning, task management, and authoring documents using plain text with a simple and expressive markup syntax. It supports hierarchical outlines, TODO lists, scheduling, deadlines, time tracking, and exporting to multiple formats including HTML, LaTeX, PDF, and Markdown.
- **mod-org-appear**: Org-appear temporarily reveals normally hidden elements (such as emphasis markers, links, or entities) when the cursor enters them, and hides them again when the cursor leaves.
* **mod-default-settings**: Configure enhanced default settings, including improved defaults, backup files, warnings to ignore, a minibuffer depth indicator, window behavior...
- **mod-helpful**: Helpful enhances the built-in help system by providing richer, more contextual information about symbols, functions, variables, and macros. Unlike the default `describe-*` commands, Helpful integrates additional metadata such as source code, documentation strings, keybindings, references, and interactive examples, all within a unified and easily navigable buffer.
- **mod-elec-pair**: Automatically insert matching delimiters (), {}...
- **mod-paren**: `show-paren-mode` allows one to see matching pairs of parentheses and other characters. When point is on the opening character of one of the paired characters, the other is highlighted. When the point is after the closing character of one of the paired characters, the other is highlighted.
- **mod-gcmh**: Gcmh (Garbage Collector Magic Hack) optimizes Emacs’ garbage collection behavior by adjusting the garbage collection threshold dynamically. Instead of collecting memory frequently during normal editing, gcmh increases the threshold while Emacs is idle, reducing interruptions and improving perceived performance. It also restores the threshold during active usage to prevent excessive memory use. In essence, it makes Emacs feel more responsive by tuning garbage collection automatically.

### Enhanced Emacs Lisp (Elisp) Editing Experience (group-emacs-lisp)

The **group-emacs-lisp** package automatically enables the following modules whenever an Emacs Lisp file is opened (`emacs-lisp-mode-hook`):
- **mod-highlight-defined**: Enables `highlight-defined-mode`, a minor mode that highlights defined Emacs Lisp symbols.
- **mod-aggressive-indent**: Enables `aggressive-indent-mode`, a minor mode that ensures Elisp code remains consistently indented. It automatically reindents after every modification, providing greater reliability than `electric-indent-mode`.
- **mod-page-break-lines**: Enables `page-break-lines-mode`, a minor mode that visually replaces ASCII form-feed characters (typically `^L`) with horizontal lines to make page breaks easier to see, without altering the underlying text.

## Modules Disabled by Default

### Disabled by default: mod-treesit-auto (better syntax highlighting)

Module: **mod-treesit-auto**

The **mod-treesit-auto** module automatically installs and enables Tree-sitter major modes in Emacs 29 and later. If the Tree-sitter parser is unavailable or incompatible, it falls back to the original major mode. Tree-sitter is an incremental parsing system introduced in Emacs 29 that delivers precise, high-performance syntax highlighting. It supports a wide range of programming languages, including Bash, C, C++, C#, CMake, CSS, Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML, Elisp, Lua, and many others.

To enable it, add the following to the `~/.emacs.d/config.el` file:
```elisp
;; Tree-sitter is an incremental parsing system introduced in Emacs 29 that
;; provides precise, high-performance syntax highlighting. It supports a broad
;; set of programming languages, including Bash, C, C++, C#, CMake, CSS,
;; Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML,
;; Elisp, Lua, and many others. treesit-auto
(push 'treesit-auto lightemacs-modules)
```

### Disabled by default: group-evil (Vim Keybindings)

By default, [evil-mode](https://github.com/emacs-evil/evil) is disabled.

To enable it, add the following to the `~/.emacs.d/config.el` file:
```elisp
;; Enable Vim key bindings
(push 'group-evil lightemacs-modules)
```

The `group-evil` group of modules includes:
- **mod-evil**: Vim keybindings (evil and evil-collection).
- **mod-evil-commentary**: Comment or uncomment text in Normal or Visual mode by pressing `gc`.
- **mod-evil-snipe**: Provides two-character motions for rapid navigation within text, similar to Evil’s built-in `f`/`F`/`t`/`T` commands, but with incremental highlighting of potential targets as you type. By default, `s` (forward) and `S` (backward) are bound to `evil-snipe-s` and `evil-snipe-S`, respectively. **Usage:** Pressing `s` in normal mode prompts you to type two characters, then jumps the cursor to the nearest matching occurrence while highlighting all matches incrementally.
- **mod-evil-surround**: Enables text surrounding in visual state using `S<textobject>` or `gS<textobject>`. For example, selecting text and pressing `S"` will wrap it in double quotes.
* **mod-goto-chg**: Navigate to the most recent edit in the buffer using `goto-last-change` or `goto-last-change-reverse`. Commonly used in `evil-mode` for the motions `g;` and `g,`, as well as for the last-change register `.`.

### Disabled by default: An alternative terminal (mod-vterm)

The mod-vterm configures [vterm](https://github.com/akermu/emacs-libvterm) is an Emacs terminal emulator that provides a fully interactive shell experience within Emacs, supporting features such as color, cursor movement, and advanced terminal capabilities.

Unlike simpler Emacs terminal modes, `vterm` leverages the underlying libvterm C library for high-performance, accurate terminal emulation, allowing users to run shell programs, text-based applications, and REPLs seamlessly.

**You can launch `vterm` using:** `M-x vterm`

**Requirements:**

Before installing emacs-libvterm, you need to make sure you have installed
 1. GNU Emacs (>= 25.1) with [module support](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html). You can check that, by verifying that `module-file-suffix` is not `nil`.
 2. cmake (>= 3.11)
 3. libtool-bin (related issues: [#66](https://github.com/akermu/emacs-libvterm/issues/66) [#85](https://github.com/akermu/emacs-libvterm/issues/85#issuecomment-491845136))
 4. OPTIONAL: [libvterm](https://github.com/Sbozzolo/libvterm-mirror.git) (>= 0.2). This library can be found in the official repositories of most distributions (e.g., Arch, Debian, Fedora, Gentoo, openSUSE, Ubuntu). Typical names are `libvterm` (Arch, Fedora, Gentoo, openSUSE), or `libvterm-dev` (Debian, Ubuntu). If not available, `libvterm` will be downloaded during the compilation process. Some distributions (e.g. Ubuntu < 20.04, Debian < 11) have versions of `libvterm` that are too old. If you find compilation errors related to `VTERM_COLOR`, you should not use your system libvterm. See [FAQ](#frequently-asked-questions-and-problems) for more details.

### Disabled by default: Indentation bars (mod-indent-bars)

The **mod-indent-bars** configures the [indent-bars](https://github.com/jdtsmith/indent-bars) packages, which enhances code readability by providing visual indentation guides, optimized for speed and customization. (Useful for Yaml or Python files.)

<img width="600" src="https://github.com/jdtsmith/indent-bars/assets/93749/0eaa0d85-0893-4893-8a56-a63ab6eeac1c"/>

It supports both space and tab-based indentation and offers optional tree-sitter integration, which includes features like scope focus. The appearance of the guide bars is highly customizable, allowing you to adjust their color, blending, width, position, and even apply a zigzag pattern.

It can be enabled interactively with `M-x indent-bars-mode` or set to load automatically. For instance, add the following to your `~/.emacs.d/config.el` to enable it for Python and YAML files:

```elisp
;; Enable indent-bars-mode automatically for Python files
(add-hook 'python-ts-mode-hook #'indent-bars-mode)
(add-hook 'python-mode-hook #'indent-bars-mode)

;; Enable indent-bars-mode automatically for YAML files
(add-hook 'yaml-mode-hook #'indent-bars-mode)
(add-hook 'yaml-ts-mode-hook #'indent-bars-mode)
```

(By default, Lightemacs sets `indent-bars-prefer-character` to `t` because it is more reliable and compatible with a wider range of configurations. If [stipples](https://github.com/jdtsmith/indent-bars?tab=readme-ov-file#stipples) render correctly on your system, you can set `indent-bars-prefer-character` to `nil`.)


## Other Features

In addition to modules, Lightemacs provides the following features:
- Reduced clutter: A `var/` directory (e.g., `~/.emacs.d/var/`) is used to store all files that Emacs normally places in the base directory (e.g., `~/.emacs.d`). By default, Emacs stores configuration files, caches, backups, and other data within `~/.emacs.d`, which can accumulate over time and complicate management.
- Lightemacs configures `use-package` to automatically refresh Emacs package archives once per session when installing a package that isn’t already present. This allows missing packages to be installed without manually running `package-refresh-contents` and prevents repeated refreshes in the same session, resolving issues such as invalid GPG keys when installing packages after the package list has become outdated.

## Useful variables

### Ellipsis
Change the default Ellipsis using the `lightemacs-ellipsis` variable, which defaults to `" ▼"`. This string used to indicate folded sections in `org-mode`, `outline-mode`, `outline-minor-mode`... This ellipsis appears at the end of a heading or section that has been collapsed. Modify the variable in your `~/.emacs/config.el` as follows:
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

Other Emacs projects by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): The Lightemacs project is based on the minimal-emacs.d, an optimized Emacs base (init.el and early-init.el) that gives you full control over your configuration. It provides better defaults, an optimized startup, and a clean foundation for building your own vanilla Emacs setup. Building the minimal-emacs.d init.el and early-init.el was the result of extensive research and testing to fine-tune the best parameters and optimizations for an Emacs configuration.
