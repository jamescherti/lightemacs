;;; le-easysession.el --- le-easysession -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The easysession Emacs package provides a comprehensive session management for
;; Emacs. It is capable of persisting and restoring file-visiting buffers,
;; indirect buffers (clones), buffer narrowing, Dired buffers, window
;; configurations, the built-in tab-bar (including tabs, their buffers, and
;; associated windows), as well as entire Emacs frames (frame name, size,
;; position, etc.).
;;
;; With easysession, your Emacs setup is restored automatically when you
;; restart. All files, Dired buffers, and window layouts come back as they were,
;; so you can continue working right where you left off. While editing, you can
;; also switch to another session, switch back, rename sessions, or delete them,
;; giving you full control over multiple work environments.
;;
;; Easysession also supports extensions, enabling the restoration of Magit
;; buffers and the scratch buffer. Custom extensions can also be created to
;; extend its functionality.
;;
;; URL: https://github.com/jamescherti/easysession.el

;;; Code:

;;; Require

(require 'lightemacs-module)
(eval-and-compile
  (require 'lightemacs-use-package))

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
             easysession-load-including-geometry
             easysession-setup
             easysession-edit)

  :preface
  (defun le-easysession-setup ()
    "Lightemacs: Setup EasySession."
    (when lightemacs-easysession-load-session-on-startup
      (if (fboundp 'easysession-setup)
          ;; The `easysession-setup' function adds hooks:
          ;; - To enable automatic session loading during `emacs-startup-hook',
          ;;   or `server-after-make-frame-hook' when running in daemon mode.
          ;; - To automatically save the session at regular intervals, and when
          ;;   Emacs exits.
          (easysession-setup)
        ;; Legacy
        (when lightemacs-easysession-load-session-on-startup
          (if lightemacs-easysession-restore-geometry-on-startup
              ;; Including geometry
              (if (daemonp)
                  (add-hook 'server-after-make-frame-hook
                            #'easysession-load-including-geometry 102)
                (add-hook 'lightemacs-emacs-startup-hook
                          #'easysession-load-including-geometry 102))
            ;; Excluding geometry
            (if (daemonp)
                (add-hook 'server-after-make-frame-hook
                          #'easysession-load-including-geometry 102)
              (add-hook 'lightemacs-emacs-startup-hook
                        #'easysession-load-including-geometry 102))))))

    ;; Auto save mode
    (add-hook 'lightemacs-emacs-startup-hook #'easysession-save-mode 103))

  :init
  (lightemacs-module-bind easysession
    (keymap-global-set "C-c s s" #'easysession-save)
    (keymap-global-set "C-c s l" #'easysession-switch-to)  ; Load
    (keymap-global-set "C-c s L" #'easysession-switch-to-and-restore-geometry)
    (keymap-global-set "C-c s r" #'easysession-rename)
    (keymap-global-set "C-c s e" #'easysession-edit)
    (keymap-global-set "C-c s R" #'easysession-reset)
    (keymap-global-set "C-c s d" #'easysession-delete))

  (lightemacs-module-setq-maybe easysession
    ;; Customizations
    ;; easysession-save-mode-lighter-show-session-name t
    easysession-mode-line-misc-info t
    ;; Non-nil: `easysession-setup' loads the session automatically.
    ;; Nil: session is not loaded automatically; the user can load it manually.
    easysession-setup-load-session t
    ;; Priority depth used when `easysession-setup' adds `easysession' hooks.
    ;; 102 ensures that the session is loaded after all other packages.
    easysession-setup-add-hook-depth 102)

  (add-hook 'lightemacs-after-init-hook #'le-easysession-setup))

(provide 'le-easysession)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-easysession.el ends here
