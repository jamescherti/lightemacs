;;; lightemacs.el --- Lightemacs -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Useful functions. This is always loaded.

;;; Code:

;;; Variables

(defvar lightemacs-modules '(;; Default modules
                             lem-group-default-modules

                             ;; Vim keybindings (DISABLED)
                             ;; --------------------------
                             ;; lem-group-evil

                             ;; Treesitter, Better syntax highlighting (DISABLED)
                             ;; -------------------------------------------------
                             ;; lem-treesit-auto
                             )
  "Modules that are enabled by default.

Lightemacs provides a range of modules that can be selectively enabled or
disabled according to your preferences, with all modules ensuring packages are
loaded only when needed, enabling exceptionally fast, deferred startup.")

(defvar lightemacs-default-theme 'tomorrow-night-deepblue
  "Name of the default theme to load, if available.
Set this to nil to disable early theme loading.")

(defcustom lightemacs-verbose nil
  "Enable displaying messages.
When set to non-nil, this option will cause messages to be shown during the
compilation process, providing feedback on the compilation status."
  :type 'boolean
  :group 'lightemacs)

(defvar lightemacs-ellipsis " ▼"
  "String used to indicate folded sections in Org-mode and Outline-mode.
This ellipsis appears at the end of a heading or section that has been
collapsed. It provides a visual cue that more content is hidden. You can
customize this variable to use a different character or string (such as '…',
'▶', or other Unicode symbols) to match your visual preference or theme. This
variable is buffer-local in Org-mode and Outline-mode, affecting only the
display of folded text.")

(defvar lightemacs-cycle nil
  "If non-nil, enables cycling through candidates in supported plugins.
This enabled or disable cycling in plugins such as Vertico and Consult.
When nil, cycling is disabled, so selection stops at the first or last candidate
instead of wrapping around.")

;;; Modules

;; require-file/load-file are better.
;;
;; (require/load-any couldn't find some files when those files are already
;; compiled.)
(defvar lightemacs--load-module-method 'require-file)

(defun lightemacs-load-modules (modules)
  "Load all modules listed in MODULES."
  (when (boundp 'lightemacs--modules-dir)
    (let ((modules-dir lightemacs--modules-dir))
      (dolist (feature-symbol modules)
        (let* ((feature-str (format "%s" feature-symbol))
               ;; (feature-symbol (intern feature-str))
               (module-file (expand-file-name (format "%s.el" feature-str)
                                              modules-dir)))
          (when init-file-debug
            (if (file-exists-p module-file)
                (message "[lightemacs] Load: %s" feature-str)
              (message "[lightemacs] Error: The module '%s' could not be found"
                       module-file)))

          (cond
           ((eq lightemacs--load-module-method 'require)
            (require feature-symbol))

           ((eq lightemacs--load-module-method 'require-file)
            (require feature-symbol module-file))

           ((eq lightemacs--load-module-method 'load-any)
            (load (expand-file-name feature-str modules-dir)
                  nil
                  (not (bound-and-true-p init-file-debug))))

           ((eq lightemacs--load-module-method 'load-file)
            (load module-file
                  nil
                  (not (bound-and-true-p init-file-debug))
                  'nosuffix))

           (t
            (error "Invalid method for lightemacs--load-module-method %s"
                   lightemacs--load-module-method))))))))

(defun lightemacs-load-default-theme ()
  "Load the theme defined in `lightemacs-default-theme' if it is installed."
  (when (and lightemacs-default-theme
             (member lightemacs-default-theme (custom-available-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme lightemacs-default-theme t)))

(defun lightemacs-load-init-file (filename)
  "Load a file of Lisp init file named FILENAME."
  (when (boundp 'lightemacs--modules-dir)
    (load (expand-file-name (format "init/%s" filename)
                            lightemacs--modules-dir)
          nil
          (not (bound-and-true-p init-file-debug))
          'nosuffix)))

(defmacro lightemacs-verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  `(progn
     (when lightemacs-verbose
       (message (concat "[lightemacs] " ,(car args)) ,@(cdr args)))))

;;; Useful macros

(defun lightemacs-recenter-maybe (&optional arg)
  "Recenter maybe. ARG is the same argument as `recenter'."
  (when (eq (current-buffer) (window-buffer))
    (let ((point (point))
          (wend (window-end nil t))
          (wstart (window-start nil)))
      (cond
       ((or (> point wend)
            (< point wstart))
        ;; If the end of the buffer is not already on the screen, scroll to
        ;; position it near the bottom.
        (overlay-recenter (point))
        (recenter arg))))))

;;; On first input/file/buffer

(defvar lightemacs-on-first-input-hook nil
  "Transient hooks run before the first user input.")
(defvar lightemacs-on-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(defvar lightemacs-on-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")

;; `permanent-local' ensures their values persist across major mode changes.
(put 'lightemacs-on-first-input-hook 'permanent-local t)
(put 'lightemacs-on-first-file-hook 'permanent-local t)
(put 'lightemacs-on-first-buffer-hook 'permanent-local t)

(defun lightemacs--on-run-first-input-hook (&rest _)
  "Run `lightemacs-on-first-input-hook' once upon the first user input."
  (when after-init-time
    (remove-hook 'pre-command-hook #'lightemacs--on-run-first-input-hook)
    (run-hooks 'lightemacs-on-first-input-hook)))

(defun lightemacs--on-run-first-file-hook (&rest _)
  "Run `lightemacs-on-first-file-hook' once upon the first opened file."
  (when after-init-time
    (remove-hook 'find-file-hook #'lightemacs--on-run-first-file-hook)
    (remove-hook 'dired-initial-position-hook #'lightemacs--on-run-first-file-hook)
    (run-hooks 'lightemacs-on-first-file-hook)))

(defun lightemacs--on-run-first-buffer-hook (&rest _)
  "Run `lightemacs-on-first-buffer-hook' once upon the first visible buffer."
  (when after-init-time
    (remove-hook 'find-file-hook #'lightemacs--on-run-first-buffer-hook)
    (remove-hook 'window-buffer-change-functions #'lightemacs--on-run-first-buffer-hook)
    (remove-hook 'server-visit-hook #'lightemacs--on-run-first-buffer-hook)
    (run-hooks 'lightemacs-on-first-buffer-hook)))

(unless noninteractive
  (add-hook 'pre-command-hook #'lightemacs--on-run-first-input-hook)

  (add-hook 'find-file-hook #'lightemacs--on-run-first-file-hook)
  (add-hook 'dired-initial-position-hook #'lightemacs--on-run-first-file-hook)

  (add-hook 'find-file-hook #'lightemacs--on-run-first-buffer-hook)
  (add-hook 'window-buffer-change-functions #'lightemacs--on-run-first-buffer-hook)
  (add-hook 'server-visit-hook #'lightemacs--on-run-first-buffer-hook))

;;; FIX

(defun lightemacs-find-dired-parent ()
  "Open a `dired' buffer for the current file's directory and select the file.
If the buffer is not visiting a file, opens the current `default-directory'."
  (interactive)
  (when (fboundp 'dired-goto-file)
    (let* ((buf (or (buffer-base-buffer)
                    (current-buffer)))
           (file (buffer-file-name buf))
           dir)
      (if file
          (setq dir (file-name-directory file))
        (setq dir default-directory))
      (when dir
        (when-let* ((dired-buf (find-file-noselect dir)))
          (switch-to-buffer dired-buf nil t)
          (when file
            (with-current-buffer dired-buf
              (when (derived-mode-p 'dired-mode)
                (dired-goto-file file)
                (lightemacs-recenter-maybe)))))))))

;;; Provide lightemacs

(provide 'lightemacs)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs.el ends here
