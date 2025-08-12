# Lightemacs - Fast and Lightweight Emacs framework

The Lightemacs project is a fast and lightweight Emacs framework.

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

## Features and modules enabled by default

### Better minibuffer and navigation (mod-consult, mod-embark, and mod-vertico)

Vertico, Consult, and Embark collectively enhance Emacs' completion and navigation capabilities.

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

### Other features

- Reduced clutter: A `var/` directory (e.g., `~/.emacs.d/var/`) is used to store all files that Emacs normally places in the base directory (e.g., `~/.emacs.d`). By default, Emacs stores configuration files, caches, backups, and other data within `~/.emacs.d`, which can accumulate over time and complicate management.
- Ensure that all Elisp libraries undergo both byte-compilation and native-compilation via the compile-angel package.
- Enable Vim keybindings through Evil mode, including evil-surround, evil-snipe, and vim-tab-bar. These features can be easily disabled for users who prefer not to use Vim keybindings.

## Links

- [minimal-emacs.d @GitHub](https://github.com/jamescherti/minimal-emacs.d)

Other Emacs packages by the same author:
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
