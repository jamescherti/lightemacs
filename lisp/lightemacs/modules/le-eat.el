;;; le-eat.el --- le-eat -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Eat (Emulate A Terminal) is a terminal emulator implemented entirely in Emacs
;; Lisp. It supports full-screen terminal applications such as Emacs itself and
;; offers high performance, operating over three times faster than Term. Eat
;; provides advanced features like Sixel graphics, complete mouse support, shell
;; integration, and reduced screen flicker for smoother performance. Configuring
;; shell integration is recommended to fully utilize its capabilities.
;;
;; URL: https://codeberg.org/akib/emacs-eat

;;; Code:

(require 'lightemacs)
(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(defvar lightemacs-eat-optimize t
  "Non-nil means apply performance optimizations to `eat-mode' buffers.
When non-nil, `lightemacs--optimize-terminal' runs in `eat-mode-hook'.")

(lightemacs-use-package eat
  :commands (eat
             eat-eshell-mode
             eat-eshell-visual-command-mode
             eat-other-window
             eat-project
             eat-project-other-window
             eat-term-make)
  :functions eat-self-input

  :init
  (lightemacs-module-setq-maybe eat
    eat-maximum-latency 0.01
    eat-kill-buffer-on-exit t
    ;; Set the amount of characters retained by `eat'.
    eat-term-scrollback-size (* 64 1024))

  (when lightemacs-eat-optimize
    (add-hook 'eat-mode-hook #'lightemacs--optimize-terminal))

  ;; straight.el symlinks or copies only Elisp files into the build/ directory,
  ;; leaving non-Elisp resources (such as terminfo data and shell integration
  ;; scripts) behind in the repository folder.
  (when (eq lightemacs-package-manager 'straight)
    (let ((eat-repo-dir (expand-file-name "straight/repos/emacs-eat/"
                                          (or (bound-and-true-p straight-base-dir)
                                              lightemacs-var-directory))))
      (setq eat-term-shell-integration-directory (expand-file-name "integration" eat-repo-dir)
            eat-term-terminfo-directory (expand-file-name "terminfo" eat-repo-dir)))))

(provide 'le-eat)

;;; le-eat.el ends here
