;;; le-server.el --- le-server -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The Emacs server allows external programs such as `emacsclient' to connect to
;; a single running instance of Emacs. This makes it possible to open files in
;; an existing session rather than starting a new Emacs process each time.
;;
;; Once the server is running, the `emacsclient' command can be used in the
;; terminal to open files in the active Emacs session. For example, running the
;; following command opens the file in the existing Emacs frame without blocking
;; the terminal process:
;;   emacsclient -n filename.txt

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(defvar lightemacs-server-quiet lightemacs-reduce-messages
  "Non-nil means suppress messages when starting the Emacs server.")

(lightemacs-use-package server
  :ensure nil
  :if (not (daemonp))
  :commands (server-running-p
             server-start)
  :hook (lightemacs-after-init . lightemacs-server--start)
  :preface
  (defun lightemacs-server--start ()
    "Start the Emacs server if no server process is currently active.
This function checks whether a server is already running. If no active server is
detected, it initializes a new server instance."
    (unless (server-running-p)
      (let ((inhibit-message lightemacs-server-quiet))
        (server-start)))))

(provide 'le-server)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-server.el ends here
