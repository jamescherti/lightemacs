;;; lightemacs.el --- Lightweight and fast framework -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lightemacs, Lightweight and Fast Emacs Framework.

;;; Code:

;;; Variables

(defvar lightemacs-modules '(;; Default modules
                             le-group-default-modules

                             ;; Vim keybindings (DISABLED)
                             ;; --------------------------
                             ;; le-group-evil

                             ;; Treesitter, Better syntax highlighting (DISABLED)
                             ;; -------------------------------------------------
                             ;; le-treesit-auto
                             )
  "Modules that are enabled by default.

Lightemacs provides a range of modules that can be selectively enabled or
disabled according to your preferences, with all modules ensuring packages are
loaded only when needed, enabling exceptionally fast, deferred startup.")

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

(defvar lightemacs-cycle t
  "If non-nil, enables cycling through candidates in supported plugins.
This enabled or disable cycling in plugins such as Vertico and Consult.
When nil, cycling is disabled, so selection stops at the first or last candidate
instead of wrapping around.")

;;; Functions

(defmacro lightemacs-verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when lightemacs-verbose
       (message (concat "[lightemacs] " ,(car args)) ,@(cdr args)))))

;; require-file/load-file are better.
;;
;; (require/load-any couldn't find some files when those files are already
;; compiled.)

(defun lightemacs-load-modules (modules)
  "Load all modules listed in MODULES."
  (dolist (feature-symbol modules)
    (let* ((feature-str (format "%s" feature-symbol)))
      (lightemacs-verbose-message "Load: %s" feature-str)
      (require feature-symbol))))

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

;;; Find parent directory

(defun lightemacs-find-parent-directory ()
  "Open a `dired' buffer for the current file's directory and select the file.
If the buffer is not visiting a file, opens the current `default-directory'."
  (interactive)
  (let* ((buf (or (buffer-base-buffer) (current-buffer)))
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
              (when (fboundp 'dired-goto-file)
                (dired-goto-file file))
              (lightemacs-recenter-maybe))))))))


;;; Useful macros

(defmacro lightemacs-recenter-if-out-of-view (&rest body)
  "Execute BODY and recenter if point is outside the original window bounds."
  (declare (indent 0) (debug t))
  (let ((window-start (make-symbol "window-start"))
        (window-end (make-symbol "window-end")))
    `(let ((,window-start (window-start))
           (,window-end (window-end)))
       (unwind-protect
           (progn ,@body)
         (let ((point (point)))
           (when (not (and (>= point ,window-start)
                           (<= point ,window-end)))
             (recenter)))))))

(defmacro lightemacs-save-window-hscroll (&rest body)
  "Execute BODY while preserving the horizontal scroll of the selected window.

This macro saves the current `window-hscroll` of the selected window.
After BODY executes, the horizontal scroll is restored exactly, leaving
the vertical position and window start unchanged.

Use this macro when you only need to maintain horizontal alignment,
without restoring the lines above the cursor."
  (declare (indent 0) (debug t))
  (let ((window (make-symbol "window"))
        (hscroll (make-symbol "window-hscroll")))
    `(let ((,window (selected-window))
           (,hscroll (window-hscroll)))
       (unwind-protect
           (progn ,@body)
         (set-window-hscroll ,window ,hscroll)))))

(defmacro lightemacs-save-window-start (&rest body)
  "Preserve and restore `window-start' relative to the lines above the cursor.

This macro saves the first visible line in the selected window. After BODY
executes, the window is restored so that the same lines remain visible above the
cursor, maintaining the relative vertical position of the cursor within the
window.

To also restore the mark, this macro can be combined with
`save-mark-and-excursion'. For preservation of horizontal scroll only (hscroll),
consider using the `lightemacs-save-window-hscroll' macro.

Example:
  (lightemacs-save-window-hscroll
        (lightemacs-save-window-start
          (save-mark-and-excursion
            ;;; Add code here
            t))

This macro is appropriate when it is necessary to maintain the visual layout of
the buffer, particularly if BODY may scroll the window or otherwise move the
cursor."
  (declare (indent 0) (debug t))
  (let ((window (make-symbol "window"))
        (buffer (make-symbol "buffer"))
        (restore-window-settings-p (make-symbol "restore-window-settings-p"))
        (lines-before-cursor (make-symbol "lines-before-cursor")))
    `(let* ((,window (selected-window))
            (,buffer (window-buffer ,window))
            (,restore-window-settings-p (eq ,buffer
                                            (current-buffer)))
            (,lines-before-cursor nil))
       (when ,restore-window-settings-p
         (with-current-buffer ,buffer
           (setq ,lines-before-cursor (count-screen-lines
                                       (save-excursion
                                         (goto-char (window-start))
                                         (beginning-of-visual-line)
                                         (point))
                                       (save-excursion
                                         (beginning-of-visual-line)
                                         (point))
                                       nil
                                       ,window))))
       (unwind-protect
           (progn ,@body)
         (when ,restore-window-settings-p
           (set-window-start ,window
                             ;; Dotimes and (line-move-visual -1) is more accurate
                             ;; than (line-move-visual N).
                             (save-excursion
                               (dotimes (_ ,lines-before-cursor)
                                 (condition-case nil
                                     (let ((line-move-visual t)
                                           (line-move-ignore-invisible t))
                                       (line-move -1))
                                   (error nil)))

                               (beginning-of-visual-line)
                               (point))))))))

;;; Macros

(defmacro lightemacs-enable-local-mode (mode hook-list)
  "Define a minor mode hook variable and add MODE to each hook in HOOK-LIST.

Defines `lightemacs-MODE-hook-list` initialized with HOOK-LIST.
Each hook in HOOK-LIST will have MODE added via `add-hook`."
  (declare (indent 0) (debug (symbolp sexp)))
  (let ((var (intern (format "lightemacs-%s-hook-list" mode))))
    `(progn
       (defvar ,var ,hook-list
         ,(format "Hooks where `%s' is enabled." mode))
       (dolist (hook ,var)
         (add-hook hook #',mode)))))

;;; Provide lightemacs

(provide 'lightemacs)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs.el ends here
