;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lightemacs is a Fast and Lightweight Emacs Framework.

;;; Code:

;;; Update defaults

;; This will be restored later by minimal-emacs.d
(setq minimal-emacs--backup-gc-cons-threshold gc-cons-threshold)
(defvar minimal-emacs--backup-gc-cons-percentage gc-cons-percentage)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(setq load-prefer-newer t)

(setq package-enable-at-startup nil)
(setq minimal-emacs-frame-title-format "%b – Lightemacs")
(setq minimal-emacs-package-initialize-and-refresh nil)  ; Managed by Lightemacs
(setq minimal-emacs-gc-cons-percentage 0.1)
(setq minimal-emacs-gc-cons-threshold (* 40 1024 1024))
(setq minimal-emacs-gc-cons-threshold-restore-delay 3)
(setq minimal-emacs-ui-features '(context-menu tooltips))

(setq straight-recipe-overrides
      '((paredit . (:type git :host nil :repo "https://paredit.org/cgit/paredit"))
        (easysession . (:host github :repo "jamescherti/easysession.el"
                              :files (:defaults "extensions/*.el")))))

;;; Global variables

(add-to-list 'load-path (expand-file-name "lisp/lightemacs" user-emacs-directory))
(require 'le-core-paths)

;;; Update `load-path'

(eval-and-compile
  (add-to-list 'load-path lightemacs-local-modules-directory)
  (add-to-list 'load-path lightemacs-core-directory)
  (add-to-list 'load-path lightemacs-modules-directory))

;;; Load config.el

(load (expand-file-name "config" lightemacs-local-directory)
      :no-error
      (not (bound-and-true-p init-file-debug)))

;;; Load lightemacs.el

(eval-and-compile
  (require 'lightemacs))

(setq minimal-emacs-load-compiled-init-files lightemacs-load-compiled-init-files)

;;; Load pre-early-init.el

(lightemacs-load-user-init
 (expand-file-name "pre-early-init.el" lightemacs-local-directory)
 :no-error)

;;; Load minimal-emacs.d early-init.el

(lightemacs-load-user-init
 (expand-file-name "early-init.el" minimal-emacs-user-directory))

;;; Load post-early-init.el

(lightemacs-load-user-init
 (expand-file-name "post-early-init.el" lightemacs-local-directory)
 :no-error)

;;; early-init.el ends here
