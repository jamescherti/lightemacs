;;; le-easysession.el --- le-easysession -*- no-byte-compile: t; lexical-binding: t -*-

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

(require 'lightemacs)

(defvar lightemacs-easysession-save-scratch t
  "Make EasySession also save and restore the scratch buffer.")

(defvar lightemacs-easysession-load-including-geometry t)

;;; use-package easysession

(lightemacs-use-package
  easysession
  :diminish easysession-save-mode
  :commands (easysession-mode
             easysession-load
             easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry
             easysession-get-current-session-name)

  :init
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

  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)
  :config
  (setq easysession-mode-line-misc-info t))

;;; Keybindings

(lightemacs-define-keybindings easysession
  (global-set-key (kbd "C-c ss") 'easysession-save-as)
  (global-set-key (kbd "C-c sl") 'easysession-switch-to)
  (global-set-key (kbd "C-c sr") 'easysession-rename)
  (global-set-key (kbd "C-c sL") 'easysession-load)
  (global-set-key (kbd "C-c sw") 'easysession-save))

;;; Persist and restore the scratch buffer

(use-package easysession-scratch
  :if lightemacs-easysession-save-scratch
  :ensure nil
  :commands easysession-scratch-mode
  :config
  (easysession-scratch-mode))

(provide 'le-easysession)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-easysession.el ends here
