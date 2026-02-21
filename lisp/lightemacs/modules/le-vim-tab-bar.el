;;; le-vim-tab-bar.el --- Module: vim-tab-bar -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The vim-tab-bar Emacs package enhances the built-in tab-bar with a
;; minimalist, Vim-inspired design that automatically adapts to the current
;; Emacs theme as well as any themes loaded subsequently. This ensures the
;; tab-bar integrates with the rest of the Emacs interface, eliminating visual
;; conflicts and making it feel like a natural extension of your Emacs
;; environment.
;;
;; Beyond its Vim-inspired design, the vim-tab-bar package is valued by users
;; who prioritize theme consistency, as it integrates the Emacs tab-bar with any
;; Emacs theme, producing a visually coherent and polished interface.
;;
;; URL: https://github.com/jamescherti/vim-tab-bar.el

;;; Code:

(require 'lightemacs-module)

(lightemacs-use-package vim-tab-bar
  :commands vim-tab-bar-mode
  :init
  (add-hook 'lightemacs-after-init-hook #'vim-tab-bar-mode))

(provide 'le-vim-tab-bar)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-vim-tab-bar.el ends here
