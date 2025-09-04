;;; le-git-modes.el --- le-git-modes -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emacs major modes for managing Git configuration files, such as
;; .gitattributes, .gitconfig, and .gitignore.
;;
;; URL: https://github.com/magit/git-modes

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package git-modes
  :commands (gitattributes-mode
             gitconfig-mode
             gitignore-mode)
  :mode
  ("/.gitignore_global\\'" . gitignore-mode)
  ("/.gitignore\\'" . gitignore-mode)
  ("/.gitconfig\\'" . gitconfig-mode)
  ("/.gitattributes_global\\'" . gitattributes-mode)
  ("/.gitattributes\\'" . gitattributes-mode))

(provide 'le-git-modes)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-git-modes.el ends here
