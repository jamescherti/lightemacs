;;; lightemacs.el --- Lightweight and fast framework -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Lightemacs is a Fast and Lightweight Emacs Framework.

;;; Code:

;;; Require

(eval-and-compile
  (require 'use-package))

(eval-when-compile
  ;; TODO test with local files containing a different value
  (setq use-package-hook-name-suffix "-hook"))

(require 'cl-lib)
(require 'le-core-compile-mod)

;;; Variables

(defvar lightemacs-modules '(;; Default modules
                             le-flavor-essential

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

(defvar lightemacs-core-modules '(;; This loaded the default theme specified
                                  ;; in the `lightemacs-theme-name' variable.
                                  ;; The default theme tomorrow-night-deepblue
                                  ;; is a beautiful deep blue variant of the
                                  ;; Tomorrow Night theme, which is renowned
                                  ;; for its elegant color palette that is
                                  ;; pleasing to the eyes.
                                  le-theme

                                  ;; Hides or abbreviates mode indicators in
                                  ;; the Emacs mode line for a cleaner display
                                  le-diminish)
  "Core modules that are enabled by default.
Do not modify this variable.")

(defvar lightemacs-excluded-packages nil
  "List of package symbols that should be excluded from initialization.
Each element must be a symbol naming a package that would otherwise be
initialized by Lightemacs. Packages listed here are skipped during the
initialization process. Only packages declared via the `lightemacs-use-package'
macro are affected by this variable.")

(defcustom lightemacs-verbose nil
  "Enable displaying verbose messages."
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

(defvar lightemacs-use-package-refresh-contents t
  "If non-nil, `lightemacs-use-package' may refresh package contents once.
Refresh package contents when `lightemacs-use-package-refresh-contents' is
non-nil and the package is not installed.")

(defvar lightemacs-package-manager 'use-package
  "Specifies which package manager to use in Lightemacs.

Choices are:
- \='use-package: Use Emacs' built-in package.el and `use-package'.
- \='straight: Use `straight.el' for package management.
- \='elpaca: Use `elpaca'.

This variable controls how the `lightemacs-use-package' macro handles
installation and configuration of packages.")

(defvar lightemacs-native-comp-excluded-cpus nil
  "Number of CPUs to reserve and not use for `native-compile'.
Set this to nil to disable this feature.")

(defvar lightemacs-load-compiled-init-files t
  "If non-nil, attempt to load byte-compiled .elc for init files.
This will enable Lightemacs to load byte-compiled or possibly native-compiled
init files for the following initialization files: init.el, pre-init.el,
post-init.el, pre-early-init.el, and post-early-init.el.")

(defvar lightemacs-byte-compile-core t
  "Indicates whether Lightemacs source files should be byte-compiled.

When uncertain, keep this set to t. Stale .elc files may occasionally lead to
unexpected issues.

When this variable is non-nil, the Lightemacs configuration or supporting files
are automatically compiled into bytecode (.elc files). This improves loading
speed by reducing parsing overhead. If nil, files will be loaded directly from
their source form without compilation, which is useful during development or
when debugging.")

;;; Functions

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

        (when lightemacs-byte-compile-core
          (lightemacs--byte-compile-if-outdated feature-symbol))

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

;;; lightemacs-use-package

(defvar lightemacs--use-package-refreshed nil
  "Whether package contents have been refreshed for `lightemacs-use-package'.")

(defun lightemacs--before-use-package (name ensure)
  "Run this function before `lightemacs-use-package' if :ensure is non-nil.
NAME is the symbol identifying the package, and ENSURE is the `use-package'
:ensure keyword passed to `lightemacs-use-package'."
  (when (and ensure
             (not lightemacs--use-package-refreshed)
             lightemacs-use-package-refresh-contents
             (eq lightemacs-package-manager 'use-package)
             (not (package-installed-p name)))
    ;; Refresh package NAME contents once before installing a missing package.
    (lightemacs-verbose-message
      "Refresh package contents before installing %s" name)
    (package-refresh-contents)
    (setq lightemacs--use-package-refreshed t)))

(defmacro lightemacs-use-package (name &rest plist)
  "Configure an Emacs package with deferred `use-package` expansion.
NAME is the package symbol.
PLIST contains keyword arguments for `use-package`.

If `lightemacs-emacs-straight' is non-nil and :straight is not
already in PLIST, the package is installed via straight.el."
  (declare (indent defun) (debug t))
  (let ((ensure (if (plist-member plist :ensure)
                    (plist-get plist :ensure)
                  use-package-always-ensure)))
    `(lightemacs--before-use-package ',name ',ensure)
    `(use-package ,name ,@plist)))

;;; Internal functions

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

;;; Init

(when lightemacs-native-comp-excluded-cpus
  (setq native-comp-async-jobs-number
        (lightemacs--calculate-native-comp-async-jobs-number)))

;;; Provide lightemacs

(provide 'lightemacs)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:
;;; lightemacs.el ends here
