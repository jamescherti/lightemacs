;;; lightemacs.el --- Lightweight and fast framework -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lightemacs is a Fast and Lightweight Emacs Framework.

;;; Code:

;;; Require

(require 'cl-lib)
(require 'le-core-paths)
(require 'le-core-defaults)

;;; Functions

(defmacro lightemacs-verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when (or lightemacs-verbose lightemacs-debug)
       (message (concat "[lightemacs] " ,(car args)) ,@(cdr args)))))

(defmacro lightemacs-debug-message (&rest args)
  "Display a debug message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when lightemacs-debug
       (message (concat "[lightemacs DEBUG] " ,(car args)) ,@(cdr args)))))

(defvar lightemacs--load-module-method 'require)
(defvar lightemacs--loaded-modules nil)

(defun lightemacs-load-modules (modules)
  "Load all modules listed in MODULES."
  (dolist (feature-symbol modules)
    (unless (memq feature-symbol lightemacs--loaded-modules)
      (let* ((feature-str (format "%s" feature-symbol))
             ;; NOTE it returns the .elc when available
             (module-file (locate-library feature-str)))
        (lightemacs-verbose-message "Load module: %s" feature-str)

        (cond
         ((eq lightemacs--load-module-method 'require)
          (require feature-symbol))

         ((eq lightemacs--load-module-method 'require-file)
          (require feature-symbol module-file))

         ((eq lightemacs--load-module-method 'load-any)
          (load module-file
                nil  ; no-error
                (not (bound-and-true-p init-file-debug))))

         ((eq lightemacs--load-module-method 'load-file)
          (load module-file
                nil  ; no-error
                (not (bound-and-true-p init-file-debug))
                'nosuffix))

         (t
          (error
           "[lightemacs] Invalid method for lightemacs--load-module-method %s"
           lightemacs--load-module-method)))

        (push feature-symbol lightemacs--loaded-modules)))))

;;; Useful macros

(defun lightemacs-recenter-maybe (&optional arg adjust-arg)
  "Conditionally recenter the window around point.

If the point has moved outside the visible portion of the window, recenter it.
ARG is the same as the argument to `recenter': a number specifies the line
position to recenter the point, nil recenters in the middle of the window, and a
negative number counts from the bottom.

The optional argument ADJUST-ARG controls whether the function modifies the
ARG value before calling `recenter'. If ADJUST-ARG is nil, the function
passes ARG directly to `recenter' with no changes. If ADJUST-ARG is non-nil,
the function checks whether the point is above or below the visible window. When
the point is below the window, ARG is converted to a negative value so that
`recenter' places the point relative to the bottom instead of the top, which
improves visibility. When the point is above the window, the function recenters
normally. In short, ADJUST-ARG enables adaptive behavior that chooses whether
to recenter relative to the top or bottom of the window depending on the point's
location.

This function only affects the current window and does nothing if the current
buffer is not displayed in the selected window."
  (when (eq (current-buffer) (window-buffer))
    (let ((point (point))
          (wend (window-end nil t))
          (wstart (window-start nil))
          (do-recenter nil))
      (when (numberp arg)
        (setq arg (abs arg)))

      ;; If the end of the buffer is not already on the screen, scroll to
      ;; position it near the bottom.
      (overlay-recenter (point))

      (cond
       ((> point wend)
        (setq do-recenter t)
        (when (and adjust-arg arg)
          (setq arg (* -1 arg))))

       ((< point wstart)
        (setq do-recenter t)))

      (when do-recenter
        (recenter arg t)))))

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

;;; Misc macros

(defmacro lightemacs-recenter-if-out-of-view (&rest body)
  "Execute BODY and recenter if point is off-screen.

This checks if the point is visible in the selected window after
execution. If the buffer changes or the window is deleted,
recentering is skipped."
  (declare (indent 0) (debug t))
  (let ((win-sym (make-symbol "start-window")))
    `(let ((,win-sym (selected-window)))
       ;; use prog1 to return the result of body
       (prog1 (progn ,@body)
         (when (and (window-live-p ,win-sym)
                    (eq (current-buffer) (window-buffer ,win-sym))
                    ;; pos-visible-in-window-p handles partial lines and
                    ;; redisplay logic more robustly than manual arithmetic.
                    (not (pos-visible-in-window-p (point) ,win-sym)))
           (recenter))))))

(defmacro lightemacs-save-window-hscroll (&rest body)
  "Execute BODY while preserving the horizontal scroll of the selected window.

This macro saves the current `window-hscroll` of the selected window.
After BODY executes, the horizontal scroll is restored exactly, leaving
the vertical position and window start unchanged.

Use this macro when you only need to maintain horizontal alignment,
without restoring the lines above the cursor."
  (declare (indent 0) (debug t))
  (let ((window (make-symbol "window"))
        (hscroll (make-symbol "hscroll"))
        (should-restore (make-symbol "should-restore")))
    `(let* ((,window (selected-window))
            ;; Check conditions and capture scroll BEFORE body runs
            (,should-restore (and (window-live-p ,window)
                                  (eq (current-buffer) (window-buffer ,window))))
            (,hscroll (when ,should-restore
                        (window-hscroll ,window))))
       (unwind-protect
           (progn ,@body) ; Execute body exactly ONCE
         ;; Restore only if conditions were originally met
         (when (and ,should-restore (window-live-p ,window))
           (set-window-hscroll ,window ,hscroll))))))

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
        (should-restore (make-symbol "should-restore"))
        (lines-before-cursor (make-symbol "lines-before-cursor")))
    `(let* ((,window (selected-window))
            ;; Check conditions and capture scroll BEFORE body runs
            (,should-restore (and (window-live-p ,window)
                                  (eq (current-buffer) (window-buffer ,window))))
            (,buffer (window-buffer ,window))
            (,lines-before-cursor
             (when ,should-restore (count-screen-lines
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
         (when ,should-restore
           (set-window-start ,window
                             ;; Dotimes and (line-move-visual -1) is more
                             ;; accurate than (line-move-visual N).
                             (save-excursion
                               (dotimes (_ ,lines-before-cursor)
                                 (condition-case nil
                                     (let ((line-move-visual t)
                                           (line-move-ignore-invisible t)
                                           ;; Disable the "Goal Column" behavior
                                           ;; so it moves vertically
                                           (temporary-goal-column 0)
                                           (goal-column nil))
                                       (line-move -1))
                                   (error nil)))

                               (beginning-of-visual-line)
                               (point))
                             ;; noforce
                             t))))))

(defmacro lightemacs-shield-macros (&rest body)
  "Eval BODY while preventing premature macro expansion.

Use this when a form contains code to be evaluated later, and that code depends
on a macro not yet defined. If the macro treats its arguments specially, an
argument resembling a macro call might be expanded too early, breaking
evaluation. Wrapping the outer (or higher) macro in this form avoids that
problem."
  (declare (indent 0))
  `(eval '(progn ,@body) lexical-binding))

(defmacro lightemacs-shield-macros-when-compiling (feature &rest body)
  "Evaluate BODY, shielding macros only if FEATURE is not yet available.
If FEATURE is already present, expand BODY normally.
During byte-compilation, attempt to load FEATURE eagerly."
  (declare (indent 0))
  (let ((available (featurep feature)))
    (when byte-compile-current-file
      (setq available (require feature nil 'noerror)))
    (if available
        `(progn ,@body)
      `(lightemacs-shield-macros
         (progn ,@body)))))

;;; Native comp functions

(defun lightemacs--calculate-native-comp-async-jobs-number ()
  "Set `native-comp-async-jobs-number' based on the available CPUs."
  ;; The `num-processors' function is only available in Emacs >= 28.1
  (cond
   ((and (boundp 'lightemacs-native-comp-excluded-cpus)
         (fboundp 'num-processors))
    (max 1 (- (funcall 'num-processors) lightemacs-native-comp-excluded-cpus)))

   ((boundp 'native-comp-async-jobs-number)
    native-comp-async-jobs-number)

   (t
    ;; Half (See `native-comp-async-jobs-number' default value)
    0)))

;;; Hook: `lightemacs-after-init-hook'

(defvar lightemacs-after-init-hook nil
  "Hook run after LightEmacs initialization is complete.
If `lightemacs-package-manager' is elpaca, this hook runs after
`elpaca-after-init-hook'. Otherwise, it runs after `after-init-hook', similar to
Emacs standard behavior.")

;;; Hooks: On first input/file/buffer

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

;;; early-init.el and init.el functions

(defun lightemacs--remove-el-file-suffix (filename)
  "Remove the Elisp file suffix from FILENAME and return it (.el, .el.gz...)."
  (let ((suffixes (mapcar (lambda (ext) (concat ".el" ext))
                          load-file-rep-suffixes)))
    (catch 'done
      (dolist (suffix suffixes filename)
        (when (string-suffix-p suffix filename)
          (setq filename (substring filename 0 (- (length suffix))))
          (throw 'done t))))
    filename))

(defun lightemacs-load-user-init (file &optional no-error)
  "Load a file of Lisp init file named FILENAME.
If optional second arg NO-ERROR is non-nil, report no error if FILE doesn’t
exist."
  (cl-letf (((symbol-function 'minimal-emacs-load-user-init)
             (lambda (&rest _)
               nil)))
    (if (not lightemacs-load-compiled-init-files)
        (load file
              no-error
              (not (bound-and-true-p init-file-debug))
              :nosuffix)
      ;; Remove the file suffix (.el, .el.gz, etc.) to let the `load' function
      ;; select between .el and .elc files.
      (setq file (lightemacs--remove-el-file-suffix file))
      (load file no-error (not (bound-and-true-p init-file-debug))))))

;;; Provide lightemacs

(provide 'lightemacs)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; lightemacs.el ends here
