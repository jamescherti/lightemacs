;;; le-git-modes.el --- le-git-modes -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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
  (require 'le-core-package-manager))

(lightemacs-use-package git-modes
  :commands (gitattributes-mode
             gitconfig-mode
             gitignore-mode)
  :mode
  (("/\\.gitignore\\'" . gitignore-mode)
   ("/info/exclude\\'" . gitignore-mode)
   ("/git/ignore\\'" . gitignore-mode)
   ("/.gitignore_global\\'" . gitignore-mode)  ; jc-dotfiles

   ("/\\.gitconfig\\'" . gitconfig-mode)
   ("/\\.git/config\\'" . gitconfig-mode)
   ("/modules/.*/config\\'" . gitconfig-mode)
   ("/git/config\\'" . gitconfig-mode)
   ("/\\.gitmodules\\'" . gitconfig-mode)
   ("/etc/gitconfig\\'" . gitconfig-mode)

   ("/\\.gitattributes\\'" . gitattributes-mode)
   ("/info/attributes\\'" . gitattributes-mode)
   ("/git/attributes\\'" . gitattributes-mode)))

(provide 'le-git-modes)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-git-modes.el ends here
