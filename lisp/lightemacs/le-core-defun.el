;;; le-core-defun.el --- le-core-defun -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Miscellaneous functions.

;;; Code:

;;; Misc

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

(defmacro lightemacs-recenter-if-out-of-view (&rest body)
  "Execute BODY and recenter if point is off-screen.

This checks if the point is visible in the selected window after
execution. If the buffer changes or the window is deleted,
recentering is skipped."
  (declare (indent 0) (debug t))
  (let ((win-sym (make-symbol "start-window")))
    `(let ((,win-sym (selected-window)))
       ;; use prog1 to return the result of body
       (progn ,@body)
       (when (and (>= scroll-conservatively 101)
                  (window-live-p ,win-sym)
                  (eq (current-buffer) (window-buffer ,win-sym))
                  ;; pos-visible-in-window-p handles partial lines and
                  ;; redisplay logic more robustly than manual arithmetic.
                  (not (pos-visible-in-window-p (point) ,win-sym)))
         (recenter)))))

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
                                  (eq (current-buffer)
                                      (window-buffer ,window))))
            (,buffer (window-buffer ,window))
            (,lines-before-cursor
             (when ,should-restore
               (count-screen-lines
                (save-excursion
                  (goto-char (window-start ,window))
                  (beginning-of-visual-line)
                  (point))
                (save-excursion
                  (beginning-of-visual-line)
                  (point))
                nil
                ,window))))
       (unwind-protect
           (progn ,@body)
         ;; Ensure the window and buffer still exist before attempting
         ;; restoration
         (when (and ,should-restore
                    (window-live-p ,window)
                    (buffer-live-p ,buffer)
                    (eq (current-buffer) ,buffer))
           (set-window-start ,window
                             (save-excursion
                               (vertical-motion (- ,lines-before-cursor)
                                                ,window)
                               (beginning-of-visual-line)
                               (point))
                             ;; noforce
                             t))))))

;; TODO remove
;; (defmacro lightemacs-save-window-start (&rest body)
;;   "Preserve and restore `window-start' relative to the lines above the cursor.
;;
;; This macro saves the first visible line in the selected window. After BODY
;; executes, the window is restored so that the same lines remain visible above the
;; cursor, maintaining the relative vertical position of the cursor within the
;; window.
;;
;; To also restore the mark, this macro can be combined with
;; `save-mark-and-excursion'. For preservation of horizontal scroll only (hscroll),
;; consider using the `lightemacs-save-window-hscroll' macro.
;;
;; Example:
;;   (lightemacs-save-window-hscroll
;;         (lightemacs-save-window-start
;;           (save-mark-and-excursion
;;             ;;; Add code here
;;             t))
;;
;; This macro is appropriate when it is necessary to maintain the visual layout of
;; the buffer, particularly if BODY may scroll the window or otherwise move the
;; cursor."
;;   (declare (indent 0) (debug t))
;;   (let ((window (make-symbol "window"))
;;         (buffer (make-symbol "buffer"))
;;         (should-restore (make-symbol "should-restore"))
;;         (lines-before-cursor (make-symbol "lines-before-cursor")))
;;     `(let* ((,window (selected-window))
;;             ;; Check conditions and capture scroll BEFORE body runs
;;             (,should-restore (and (window-live-p ,window)
;;                                   (eq (current-buffer) (window-buffer ,window))))
;;             (,buffer (window-buffer ,window))
;;             (,lines-before-cursor
;;              (when ,should-restore (count-screen-lines
;;                                     (save-excursion
;;                                       (goto-char (window-start))
;;                                       (beginning-of-visual-line)
;;                                       (point))
;;                                     (save-excursion
;;                                       (beginning-of-visual-line)
;;                                       (point))
;;                                     nil
;;                                     ,window))))
;;        (unwind-protect
;;            (progn ,@body)
;;          (when ,should-restore
;;            (set-window-start ,window
;;                              ;; Dotimes and (line-move-visual -1) is more
;;                              ;; accurate than (line-move-visual N).
;;                              (save-excursion
;;                                (dotimes (_ ,lines-before-cursor)
;;                                  (condition-case nil
;;                                      (let ((line-move-visual t)
;;                                            (line-move-ignore-invisible t)
;;                                            ;; Disable the "Goal Column" behavior
;;                                            ;; so it moves vertically
;;                                            (temporary-goal-column 0)
;;                                            (goal-column nil))
;;                                        (line-move -1))
;;                                    (error nil)))
;;
;;                                (beginning-of-visual-line)
;;                                (point))
;;                              ;; noforce
;;                              t))))))

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
    (when (bound-and-true-p byte-compile-current-file)
      (setq available (require feature nil 'noerror)))
    (if available
        `(progn ,@body)
      `(lightemacs-shield-macros
         (progn ,@body)))))

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

;;; Compilation

(require 'bytecomp)

(defun lightemacs-find-el-files (directory)
  "Return a list of all source files in DIRECTORY."
  (let ((regex (concat "\\.el" (regexp-opt load-file-rep-suffixes) "\\'")))
    (directory-files-recursively directory regex)))

(defun lightemacs-get-elc-files (list-el-files)
  "Return a list of existing .elc files for the source files in LIST-EL-FILES."
  (let ((elc-files nil))
    (dolist (source-file list-el-files)
      (let ((elc-file (funcall (if (bound-and-true-p
                                    byte-compile-dest-file-function)
                                   byte-compile-dest-file-function
                                 #'byte-compile-dest-file)
                               source-file)))
        (when (and elc-file (file-exists-p elc-file))
          (push elc-file elc-files))))
    elc-files))

(defun lightemacs-get-eln-files (list-el-files)
  "Return a list of existing .eln files for the source files in LIST-EL-FILES."
  (let ((eln-files nil))
    (dolist (source-file list-el-files)
      (let ((eln-file (when (fboundp 'comp-el-to-eln-filename)
                        (comp-el-to-eln-filename source-file))))
        (when (and eln-file (file-exists-p eln-file))
          (push eln-file eln-files))))
    eln-files))

(defun lightemacs-delete-elc-files (list-el-files)
  "Delete all .elc files associated with the source files in LIST-EL-FILES.
Returns a list of the files that were successfully deleted."
  (let ((elc-files (lightemacs-get-elc-files list-el-files))
        (deleted-files nil))
    (dolist (file elc-files)
      (condition-case nil
          (progn
            ;; (delete-file file)
            (message "DEL: %s" file)
            (push file deleted-files))
        (error nil)))
    deleted-files))

(defun lightemacs-delete-eln-files (list-el-files)
  "Delete all .eln files associated with the source files in LIST-EL-FILES.
Returns a list of the files that were successfully deleted."
  (let ((eln-files (lightemacs-get-eln-files list-el-files))
        (deleted-files nil))
    (dolist (file eln-files)
      (condition-case nil
          (progn
            ;; (delete-file file)
            (message "DEL: %s" file)
            (push file deleted-files))
        (error nil)))
    deleted-files))

(defun lightemacs-get-all-el-files (&optional loaded-only)
  "Get a list of all Lightemacs *.el files, including init and directory files.
If LOADED-ONLY is non-nil, return only the files that are currently loaded."
  (let ((all-el-files nil))
    ;; Collect standard init files
    (when (and user-init-file (file-exists-p user-init-file))
      (push user-init-file all-el-files))

    (let ((early-init (if (boundp 'early-init-file)
                          early-init-file
                        (expand-file-name "early-init.el"
                                          user-emacs-directory))))
      (when (file-exists-p early-init)
        (push early-init all-el-files)))

    ;; Collect files from modules directory
    (when (and (boundp 'lightemacs-modules-directory)
               (stringp lightemacs-modules-directory)
               (file-directory-p lightemacs-modules-directory))
      (setq all-el-files
            (append all-el-files
                    (lightemacs-find-el-files lightemacs-modules-directory))))

    ;; Collect files from local directory
    (when (and (boundp 'lightemacs-local-directory)
               (stringp lightemacs-local-directory)
               (file-directory-p lightemacs-local-directory))
      (setq all-el-files
            (append all-el-files
                    (lightemacs-find-el-files lightemacs-local-directory))))

    (if loaded-only
        (let ((loaded-hash (make-hash-table :test 'equal))
              (filtered-files nil))
          (dolist (lh load-history)
            (let ((lh-file (car lh)))
              (when (stringp lh-file)
                (let ((base lh-file))
                  (dolist (suffix load-file-rep-suffixes)
                    (when (and (> (length suffix) 0)
                               (string-suffix-p suffix base))
                      (setq base (substring base 0 (- (length suffix))))))
                  (when (string-match-p "\\.elc?\\'" base)
                    (setq base (file-name-sans-extension base)))
                  (puthash (file-truename base) t loaded-hash)))))
          (dolist (file all-el-files)
            (let ((base file))
              (when (string-match-p "\\.el\\'" base)
                (setq base (file-name-sans-extension base)))
              (when (gethash (file-truename base) loaded-hash)
                (push file filtered-files))))
          (nreverse filtered-files))
      all-el-files)))

(defun lightemacs-compile-all-files ()
  "Byte-compile and native compile Lightemacs core, modules, and init files.
Returns a list of the files that were compiled."
  (interactive)
  (let ((all-el-files (lightemacs-get-all-el-files :loaded-only))
        (compiled-files nil))
    (when all-el-files
      (dolist (file all-el-files)
        (let ((compiled nil))
          ;; Byte-compile
          (condition-case err
              (progn
                ;; (byte-compile-file file)
                (setq compiled t))
            (error
             (message "Byte-compile failed for %s: %s"
                      file (error-message-string err))))

          ;; Native compile
          (when (and (native-comp-available-p)
                     (fboundp 'native-compile))
            (let ((eln-file (when (fboundp 'comp-el-to-eln-filename)
                              (comp-el-to-eln-filename file))))
              (when eln-file
                (condition-case err
                    (progn
                      ;; (native-compile-async file)
                      (setq compiled t))
                  (error
                   (message "Native-compile failed for %s: %s"
                            file (error-message-string err)))))))

          (when compiled
            (push file compiled-files))))
      (message "Finished compiling %d files." (length compiled-files)))
    compiled-files))

;; (defun lightemacs-delete-all-compiled-files ()
;;   "Delete all Lightemacs core and modules .elc and .eln files."
;;   (interactive)
;;   (let* ((all-el-files (lightemacs-get-all-el-files))
;;          (deleted nil))
;;     (when all-el-files
;;       (setq deleted (append (lightemacs-delete-elc-files all-el-files)
;;                             (lightemacs-delete-eln-files all-el-files)))
;;       (if deleted
;;           (message "Successfully cleaned up %d compiled files." (length deleted))
;;         (message "No compiled files were found to delete.")))
;;     deleted))

(provide 'le-core-defun)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-core-defun.el ends here
