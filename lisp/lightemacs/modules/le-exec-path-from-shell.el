;;; le-exec-path-from-shell.el --- le-exec-path-from-shell -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configures `exec-path-from-shell' to import environment variables, such as
;; `PATH', from a login shell into Emacs. This allows the editor to locate
;; shell-configured executables when launched from a desktop environment.
;;
;; URL: https://github.com/purcell/exec-path-from-shell

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package exec-path-from-shell
  :demand t
  :functions exec-path-from-shell-initialize
  :config
  (exec-path-from-shell-initialize))

(provide 'le-exec-path-from-shell)

;;; le-exec-path-from-shell.el ends here
