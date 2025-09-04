;;; le-easysession.el --- le-easysession -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The easysession is a session manager for Emacs that can persist and restore
;; file editing buffers, indirect buffers/clones, Dired buffers, windows/splits,
;; the built-in tab-bar (including tabs, their buffers, and windows), and Emacs
;; frames. It offers a convenient and effortless way to manage Emacs editing
;; sessions and utilizes built-in Emacs functions to persist and restore frames.
;;
;; With easysession, your Emacs setup is restored automatically when you
;; restart. All files, Dired buffers, and window layouts come back as they were,
;; so you can continue working right where you left off.
;;
;; You can
;;
;; URL: https://github.com/jamescherti/easysession.el

;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs))

;; (defvar lightemacs-easysession-save-scratch t
;;   "Make EasySession also save and restore the scratch buffer.")

(defvar lightemacs-easysession-load-including-geometry t)

;;; use-package easysession

(lightemacs-use-package easysession
  :commands (easysession-rename
             easysession-reset
             easysession-save
             easysession-save-as
             easysession-save-mode
             easysession-switch-to
             easysession-delete
             easysession-get-session-name
             easysession-load
             easysession-switch-to-and-restore-geometry
             easysession-load-including-geometry)

  :bind (("C-c ss" . easysession-save-as)
         ("C-c sl" . easysession-switch-to)
         ("C-c sr" . easysession-rename)
         ("C-c sL" . easysession-load)
         ("C-c sd" . easysession-delete)
         ("C-c sS" . easysession-save))

  :init
  ;; Load session
  (if lightemacs-easysession-load-including-geometry
      ;; Including geometry
      (if (daemonp)
          (add-hook 'server-after-make-frame-hook
                    #'easysession-load-including-geometry 102)
        (add-hook 'emacs-startup-hook
                  #'easysession-load-including-geometry 102))
    ;; Excluding geometry
    (if (daemonp)
        (add-hook 'server-after-make-frame-hook
                  #'easysession-load-including-geometry 102)
      (add-hook 'emacs-startup-hook
                #'easysession-load-including-geometry 102)))

  ;; Auto save mode
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)

  (setq easysession-mode-line-misc-info t))

;; (when lightemacs-easysession-save-scratch
;;   (lightemacs-use-package
;;     easysession-scratch
;;     :if lightemacs-easysession-save-scratch
;;     :ensure nil
;;     :after easysession
;;     :config
;;     (easysession-scratch-mode)))

(provide 'le-easysession)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-easysession.el ends here
