;;; le-savehist.el --- le-savehist -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package savehist
  :ensure nil
  :commands savehist-mode
  :hook (after-init . savehist-mode))

(provide 'le-savehist)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-savehist.el ends here
