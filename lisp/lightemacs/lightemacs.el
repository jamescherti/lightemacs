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
  "Enable displaying verbose messages.
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

(defvar lightemacs-use-package-disabled-packages nil
  "A list of package symbols that are disabled.
Packages listed here will be ignored by `lightemacs-use-package', preventing
their declaration and configuration.")

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

This variable controls how `lightemacs-use-package' handles installation and
configuration of packages.")

(defvar lightemacs-straight-recipes
  ;; Correct the Paredit repository in the MELPA recipe because the default URL
  ;; is invalid.
  '((paredit . (:type git :host nil :repo "https://paredit.org/cgit/paredit"))
    ;; Add the `easysession' extensions/ directory
    (easysession . (:fetcher github :repo "jamescherti/easysession.el"
                             :files (:defaults
                                     "extensions/*.el"))))
  "Alist of packages and their custom straight.el recipes.
This is applied when `lightemacs-package-manager' is \='straight.")

(defvar lightemacs-load-compiled-init-files t
  "If non-nil, attempt to load byte-compiled .elc for init files.
This will enable Lightemacs to load byte-compiled or possibly native-compiled
init files for the following initialization files: init.el, pre-init.el,
post-init.el, pre-early-init.el, and post-early-init.el.")

(defvar lightemacs-native-comp-excluded-cpus 2
  "Number of CPUs to reserve and not use for `native-compile'.
Set this to nil to disable this feature.")

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

;; (defun lightemacs-load-modules (modules)
;;   "Load all modules listed in MODULES."
;;   (dolist (feature-symbol modules)
;;     (let* ((feature-str (format "%s" feature-symbol)))
;;       (lightemacs-verbose-message "Load: %s" feature-str)
;;       (require feature-symbol))))

(defvar lightemacs--load-module-method 'require)
(defvar lightemacs--loaded-modules nil)

(defun lightemacs-load-modules (modules)
  "Load all modules listed in MODULES."
  (when (boundp 'lightemacs-modules-directory)
    (let ((modules-dir lightemacs-modules-directory))
      (dolist (feature-symbol modules)
        (unless (memq feature-symbol lightemacs--loaded-modules)
          (let* ((feature-str (format "%s" feature-symbol))
                 (module-file
                  (when (or (eq lightemacs--load-module-method 'require-file)
                            (eq lightemacs--load-module-method 'load-file))
                    (expand-file-name (format "%s.el" feature-str)
                                      modules-dir))))
            (lightemacs-verbose-message "Load: %s" feature-str)

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
                     lightemacs--load-module-method)))

            (push feature-symbol lightemacs--loaded-modules)))))))

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

;;; Modules Macros

(defmacro lightemacs-define-keybindings (module &rest body)
  "Define key bindings for MODULE with BODY, unless inhibited.
This macro introduces an inhibition variable named:
`lightemacs-MODULE-inhibit-keybindings'.
When non-nil, BODY will not be evaluated, thereby preventing the installation of
the specified key bindings."
  (declare (indent 1) (debug t))
  (let ((inhibit-var (intern (format "lightemacs-%s-inhibit-keybindings" module))))
    `(progn
       (defvar ,inhibit-var nil
         ,(format
           "Prevent configuring `%s' keybindings.

When this variable is set to a non-nil value, any key bindings that would
normally be defined through `lightemacs-define-*' macros are skipped
for `%s'.

This allows users to disable or override the default Lightemacs key
configuration for that mode without modifying the macro definition itself."
           module
           module))
       (unless ,inhibit-var
         ,@body))))

(defmacro lightemacs-define-mode-hook-list (mode hook-list)
  "Define a minor mode hook variable and add MODE to each hook in HOOK-LIST.

Defines `lightemacs-MODE-hook-list' initialized with HOOK-LIST.
Each hook in HOOK-LIST will have MODE added via `add-hook'."
  (declare (indent 0) (debug (symbolp sexp)))
  (let ((var (intern (format "lightemacs-%s-hook-list" mode))))
    `(progn
       (defvar ,var ,hook-list
         ,(format "Hooks where `%s' is enabled." mode))
       (dolist (hook ,var)
         (add-hook hook ',mode)))))

;;; lightemacs-use-package

(defvar lightemacs--use-package-refreshed nil
  "Whether package contents have been refreshed for `lightemacs-use-package'.")

(defun lightemacs--before-use-package (name args)
  "Run this function before `lightemacs-use-package' if :ensure is non-nil.
NAME is the symbol identifying the package, and ARGS is the plist of keywords
passed to `lightemacs-use-package'."
  (when (eq lightemacs-package-manager 'use-package)
    (when-let* ((ensure (cond
                         ((memq :ensure args)
                          (plist-get args :ensure))

                         (t
                          use-package-always-ensure))))
      ;; Refresh package NAME contents once before installing a missing package.
      (when lightemacs-use-package-refresh-contents
        (when (and (not lightemacs--use-package-refreshed)
                   (not (package-installed-p name)))
          (lightemacs-verbose-message
            "Refresh package contents before installing %s"
            name)
          (package-refresh-contents)
          (setq lightemacs--use-package-refreshed t))))))

(defmacro lightemacs-use-package (name &rest plist)
  "Configure an Emacs package, skipping it if disabled or unavailable.
NAME is the package symbol.
PLIST contains keyword arguments for `use-package'.

If `lightemacs-emacs-straight' is non-nil and :straight is not already in PLIST,
the package is installed via straight.el. If a custom recipe exists in
`lightemacs-straight-recipes', it is used instead of t.

`use-package' expansion is deferred until runtime."
  (declare (indent 0) (debug t))
  (unless (or (memq name lightemacs-use-package-disabled-packages)
              (and (bound-and-true-p byte-compile-current-file)
                   (not (locate-library (symbol-name name)))))
    (let* ((straight-spec (and (eq lightemacs-package-manager 'straight)
                               (not (plist-member plist :straight))
                               (or (cdr (assoc name lightemacs-straight-recipes))
                                   t)))
           (ensure (cond
                    ((memq :ensure plist)
                     (plist-get plist :ensure))

                    (t
                     use-package-always-ensure)))
           (final-plist (if (and straight-spec
                                 ensure)
                            (append `(:straight ,straight-spec) plist)
                          plist)))
      (lightemacs--before-use-package name plist)
      `(eval '(use-package ,name ,@final-plist)))))

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

(require 'cl-lib)
(defun lightemacs-load-user-init (file &optional no-error)
  "Load a file of Lisp init file named FILENAME.
If optional second arg NO-ERROR is non-nil,
report no error if FILE doesn’t exist."
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
