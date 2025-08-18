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
            (let ((load-path (cons lightemacs--modules-dir load-path)))
              (require feature-symbol)))

           ((eq lightemacs--load-module-method 'require-file)
            (let ((load-path (cons lightemacs--modules-dir load-path)))
              (require feature-symbol module-file)))

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
