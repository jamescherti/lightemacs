;;; le-flymake-ansible-lint.el --- le-flymake-ansible-lint -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The flymake-ansible-lint package provides a Flymake backend for ansible-lint,
;; enabling real-time syntax and style checking for Ansible playbooks and roles
;; within Emacs.
;;
;; URL: https://github.com/jamescherti/flymake-ansible-lint.el

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'lightemacs-module)

(lightemacs-use-package flymake-ansible-lint
  :commands flymake-ansible-lint-setup
  :init
  (lightemacs-module-hooks flymake-ansible-lint
    flymake-ansible-lint-setup
    '(ansible-mode-hook)))

(provide 'le-flymake-ansible-lint)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-flymake-ansible-lint.el ends here
