;;; le-easysession.el --- le-easysession -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
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
;; URL: https://github.com/jamescherti/easysession.el

;;; Code:

;;; Require

(require 'lightemacs-module)

(defvar lightemacs-easysession-load-session-on-startup t
  "If non-nil, automatically load the main session when Emacs starts.
This variable controls whether `easysession' restores the main session on
startup. Set to nil to disable automatic session loading.")

(defvar lightemacs-easysession-restore-geometry-on-startup t
  "If non-nil, restore window geometry (size/position) when loading a session.
This works in conjunction with `lightemacs-easysession-load-session-on-startup'.
Set to nil to ignore window size and position during session restoration.")

;;; use-package easysession

(lightemacs-use-package easysession
  :commands (easysession-rename
             easysession-reset
             easysession-save
             easysession-save-mode
             easysession-switch-to
             easysession-delete
             easysession-get-session-name
             easysession-load
             easysession-switch-to-and-restore-geometry
             easysession-load-including-geometry)

  :bind (("C-c ss" . easysession-save)
         ("C-c sl" . easysession-switch-to)  ; Load
         ("C-c sL" . easysession-switch-to-and-restore-geometry)
         ("C-c sr" . easysession-rename)
         ("C-c se" . easysession-edit)
         ("C-c sR" . easysession-reset)
         ("C-c sd" . easysession-delete))

  :preface
  (defun le-easysession-setup ()
    "Lightemacs: Setup EasySession."
    (if (fboundp 'easysession-setup)
        ;; The `easysession-setup' function adds hooks:
        ;; - To enable automatic session loading during `emacs-startup-hook', or
        ;;   `server-after-make-frame-hook' when running in daemon mode.
        ;; - To automatically save the session at regular intervals, and when Emacs
        ;;   exits.
        (easysession-setup)
      ;; Legacy
      (when lightemacs-easysession-load-session-on-startup
        (if lightemacs-easysession-restore-geometry-on-startup
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
                      #'easysession-load-including-geometry 102))))
      ;; Auto save mode
      (add-hook 'emacs-startup-hook #'easysession-save-mode 103)))

  :init
  ;; Customizations
  ;; (setq easysession-save-mode-lighter-show-session-name t)
  (setq easysession-mode-line-misc-info t)

  ;; Non-nil: `easysession-setup' loads the session automatically.
  ;; Nil: session is not loaded automatically; the user can load it manually.
  (setq easysession-setup-load-session t)

  ;; Priority depth used when `easysession-setup' adds `easysession' hooks.
  ;; 102 ensures that the session is loaded after all other packages.
  (setq easysession-setup-add-hook-depth 102)

  (add-hook 'lightemacs-after-init-hook #'le-easysession-setup))

(provide 'le-easysession)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-easysession.el ends here
