;;; le-nerd-icons.el --- le-nerd-icons -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Nerd-icons is an icon library that provides glyphs and symbols from Nerd
;; Fonts across Emacs buffers, modes, and completion interfaces.
;;
;; Unlike earlier packages that depend on multiple disparate icon fonts,
;; nerd-icons uses the unified Nerd Fonts specification. This provides access to
;; extensive collections of file-type markers, programming language emblems,
;; directory glyphs, and UI badges through a single font family.
;;
;; URL: https://github.com/rainstormstudio/nerd-icons.el

;;; Code:

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package nerd-icons
  :commands (nerd-icons-icon-for-buffer
             nerd-icons-icon-for-dir
             nerd-icons-icon-for-extension
             nerd-icons-icon-for-file
             nerd-icons-icon-for-mode
             nerd-icons-icon-for-url
             nerd-icons-insert
             nerd-icons-install-fonts))

(provide 'le-nerd-icons)

;;; le-nerd-icons.el ends here
