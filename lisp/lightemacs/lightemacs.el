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

(require 'bytecomp)
(require 'le-core-process-buffers)

;;; Variables

(defvar lightemacs-user-directory user-emacs-directory
  "Directory beneath Lightemacs files are placed.")

;; Will be updated in early-init.el
(defvar lightemacs-local-lisp-directory user-emacs-directory)
(defvar lightemacs-var-directory user-emacs-directory)
(defvar lightemacs-builtin-lisp-directory user-emacs-directory)
(defvar lightemacs-builtin-modules-directory user-emacs-directory)
(defvar lightemacs-local-modules-directory user-emacs-directory)
(defvar lightemacs-lisp-directory user-emacs-directory)

(defvar lightemacs-modules '(;; Default modules
                             le-flavor-big

                             ;; Vim keybindings (DISABLED)
                             ;; --------------------------
                             ;; le-group-evil
                             )
  "Modules that are enabled by default.

Lightemacs provides a range of modules that can be selectively enabled or
disabled according to your preferences, with all modules ensuring packages are
loaded only when needed, enabling exceptionally fast, deferred startup.")

(defvar lightemacs-verbose nil
  "Enable displaying verbose messages.")

(defvar lightemacs-debug nil
  "Enable displaying debug messages.")

(defvar lightemacs-reduce-messages nil
  "When non-nil, reduce the verbosity of various Emacs packages.")

(defvar lightemacs-ellipsis " ▼"
  "String used to indicate folded sections.

This string indicate folded sections in Org-mode, Outline-mode, treesit-fold...

It appears at the end of a heading or section that has been collapsed. It
provides a visual cue that more content is hidden. You can customize this
variable to use a different character or string (such as '...', '▶', or similar
Unicode symbols) to match your visual preference or theme. This variable is
buffer-local in Org-mode and Outline-mode, affecting only the
display of folded text.")

(defvar lightemacs-cycle t
  "If non-nil, enables cycling through candidates in supported plugins.
This enabled or disable cycling in plugins such as Vertico and Consult. When
nil, cycling is disabled, so selection stops at the first or last candidate
instead of wrapping around.")

(defvar lightemacs-package-manager nil
  "Specifies which package manager to use in Lightemacs.

Choices are:
- \='builtin-package: Use Emacs' built-in package.el and `use-package'.
- \='straight: Use `straight.el' for package management.
- \='elpaca: Use `elpaca'.

This variable controls how the `lightemacs-module' macro handles
installation and configuration of packages.")

(defvar lightemacs-load-compiled-init-files t
  "If non-nil, attempt to load byte-compiled .elc for init files.
This will enable Lightemacs to load byte-compiled or possibly native-compiled
init files for the following initialization files: init.el, pre-init.el,
post-init.el, pre-early-init.el, and post-early-init.el.")

;;; Hook: `lightemacs-after-init-hook'

(defvar lightemacs-after-init-hook nil
  "Hook run after LightEmacs initialization is complete.
If `lightemacs-package-manager' is elpaca, this hook runs after
`elpaca-after-init-hook'. Otherwise, it runs after `after-init-hook', similar to
Emacs standard behavior.")

(defvar lightemacs-emacs-startup-hook nil
  "Hook run after Emacs startup processes are complete.
If `lightemacs-package-manager' is elpaca, this hook runs after
`elpaca-after-init-hook'. Otherwise, it runs after `emacs-startup-hook', similar
to Emacs standard behavior.")

(defvar lightemacs-text-editing-modes '(prog-mode text-mode conf-mode diff-mode)
  "List of major modes in which text editing global modes should be enabled.")

;;; Hooks: Initializer hooks

(defvar lightemacs-pre-early-init-hook nil
  "Hook run before early-init.el execution.")

(defvar lightemacs-post-early-init-hook nil
  "Hook run after early-init.el execution.")

(defvar lightemacs-pre-init-hook nil
  "Hook run before init.el execution.")

(defvar lightemacs-before-modules-hook nil
  "Hook run before loading modules in init.el.")

(defvar lightemacs-after-modules-hook nil
  "Hook run after loading modules in init.el.")

(defvar lightemacs-byte-and-native-compile-after-load t
  "Non-nil means byte-compile eligible Lisp source files after loading them.
When non-nil, Files under `lightemacs-lisp-directory' are always byte-compiled,
including those in subdirectories. Use
`lightemacs-byte-compile-additional-directories' to allow source files in other
directory trees.

Byte compilation can improve execution speed on subsequent loads. When Emacs
loads the resulting .elc file, its just-in-time (JIT) native compiler can
compile the Lisp source to machine code for faster execution.

Compilation runs only if the destination is writable and the bytecode is missing
or older than the source.

When `compile-angel-on-load-mode' is active, Lightemacs leaves compilation to
Compile Angel.")

(defvar lightemacs-byte-compile-additional-directories nil
  "Additional directories where Lightemacs may byte-compile loaded sources.
Each entry must be an absolute directory name and includes its subdirectories.
Lightemacs always allows `lightemacs-lisp-directory' and its subdirectories, so
you only need to list directories outside that tree.")

;;; Misc functions

(defun lightemacs--message (prefix format-string &rest args)
  "Display FORMAT-STRING with PREFIX and ARGS using `message'.
If FORMAT-STRING is nil, clear the echo area."
  (apply #'message (and format-string (concat prefix format-string)) args))

(defmacro lightemacs-verbose-message (format-string &rest args)
  "Display FORMAT-STRING with ARGS when verbose or debug logging is enabled.
Evaluate FORMAT-STRING and ARGS only when logging is enabled."
  (declare (indent 0) (debug (form &rest form)))
  `(when (or lightemacs-verbose lightemacs-debug)
     (lightemacs--message "[lightemacs] " ,format-string ,@args)))

(defmacro lightemacs-debug-message (format-string &rest args)
  "Display FORMAT-STRING with ARGS when debug logging is enabled.
Evaluate FORMAT-STRING and ARGS only when logging is enabled."
  (declare (indent 0) (debug (form &rest form)))
  `(when lightemacs-debug
     (lightemacs--message "[lightemacs DEBUG] " ,format-string ,@args)))

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
          (win-end (window-end nil t))
          (win-start (window-start))
          (do-recenter nil))
      (cond
       ;; `win-end', the character position returned by `window-end', is the
       ;; first position that is completely off-screen. If the point equals
       ;; window-end, it is no longer visible and requires recentering.
       ((and (>= point win-end)
             (or (< win-end (point-max))
                 (not (pos-visible-in-window-p point nil t))))
        (setq do-recenter t)
        ;; If `adjust-arg' is nil and the caller intentionally passes a negative
        ;; arg, this breaks the documented behavior of recenter. The absolute
        ;; value conversion should only be applied when adjust-arg is non-nil.
        (when (and adjust-arg (numberp arg))
          (setq arg (- (abs arg)))))

       ((< point win-start)
        (setq do-recenter t)
        (when (and adjust-arg (numberp arg))
          (setq arg (abs arg)))))

      (when do-recenter
        (recenter arg t)))))

(defmacro lightemacs-save-window-scroll (&rest body)
  "Execute BODY while preserving the horizontal and vertical scroll."
  (declare (indent 0) (debug t))
  (let ((window (make-symbol "window"))
        (window-buffer (make-symbol "window-buffer"))
        (hscroll (make-symbol "hscroll"))
        (vscroll (make-symbol "vscroll"))
        (should-restore (make-symbol "should-restore")))
    `(let* ((,window (selected-window))
            (,window-buffer (window-buffer ,window))
            ;; Check conditions and capture scroll BEFORE body runs
            (,should-restore (and (window-live-p ,window)
                                  (eq (current-buffer)
                                      (window-buffer ,window))))
            (,hscroll (when ,should-restore
                        (window-hscroll ,window)))
            (,vscroll (when ,should-restore
                        (window-vscroll ,window t))))
       (unwind-protect
           ;; Execute body exactly ONCE
           (progn ,@body)
         ;; Restore only if conditions were originally met
         (when (and ,should-restore
                    (window-live-p ,window)
                    (buffer-live-p ,window-buffer)
                    (eq ,window-buffer (window-buffer ,window)))
           (set-window-vscroll ,window ,vscroll t)
           (set-window-hscroll ,window ,hscroll))))))

(defmacro lightemacs-save-window-start (&rest body)
  "Preserve and restore `window-start' relative to the lines above the cursor.

This macro saves the first visible line in the selected window. After BODY
executes, the window is restored so that the same lines remain visible above the
cursor, maintaining the relative vertical position of the cursor within the
window.

To also restore the mark, this macro can be combined with
`save-mark-and-excursion'. For preservation of horizontal and vertical scroll,
consider using the `lightemacs-save-window-scroll' macro.

This macro is appropriate when it is necessary to maintain the visual layout of
the buffer, especially if BODY may scroll the window or otherwise move the
cursor."
  (declare (indent 0) (debug t))
  (let ((window (make-symbol "window"))
        (window-buffer (make-symbol "window-buffer"))
        (lines-before-cursor (make-symbol "lines-before-cursor"))
        (start-pos (make-symbol "start-pos")))
    `(let* ((,window (selected-window))
            (,window-buffer (window-buffer ,window))
            (,lines-before-cursor
             (when (and (window-live-p ,window)
                        (eq (current-buffer) ,window-buffer))
               (count-screen-lines
                (save-excursion
                  (goto-char (window-start ,window))
                  (vertical-motion 0)
                  (point))
                (save-excursion
                  (vertical-motion 0)
                  (point))
                nil
                ,window))))
       (unwind-protect
           (progn ,@body)
         (when (and ,lines-before-cursor
                    (window-live-p ,window)
                    (buffer-live-p ,window-buffer)
                    (eq ,window-buffer (window-buffer ,window)))
           (with-selected-window ,window
             (let ((,start-pos (save-excursion
                                 (vertical-motion 0)
                                 (vertical-motion (- ,lines-before-cursor)
                                                  ,window)
                                 (vertical-motion 0)
                                 (point))))
               (set-window-start ,window
                                 ,start-pos
                                 ;; No force
                                 t))))))))

;;; early-init.el and init.el functions

(defun lightemacs--remove-el-file-suffix (filename)
  "Remove the Elisp suffix from FILENAME and return it (.el, .elc, .el.gz...).
FILENAME is the full or relative path of the file as a string."
  (let ((suffixes (apply #'append
                         (mapcar (lambda (ext)
                                   (list (concat ".elc" ext)
                                         (concat ".el" ext)))
                                 load-file-rep-suffixes))))
    (catch 'done
      (dolist (suffix suffixes)
        (when (string-suffix-p suffix filename)
          (setq filename (substring filename 0 (- (length suffix))))
          (throw 'done t))))
    filename))

(defun lightemacs-load-user-init (file &optional no-error)
  "Load a file of Lisp init file named FILENAME.
If optional second arg NO-ERROR is non-nil, report no error if FILE doesn't
  exist."
  (if lightemacs-load-compiled-init-files
      (progn
        ;; Remove the file suffix (.el, .el.gz, etc.) to let the `load' function
        ;; select between .el and .elc files.
        (setq file (lightemacs--remove-el-file-suffix file))
        (load file no-error (not (bound-and-true-p init-file-debug))))
    (load file
          no-error
          (not (bound-and-true-p init-file-debug))
          :nosuffix)))

;;; Hooks: On first input/file/buffer

(defvar lightemacs-on-first-input-hook nil
  "Transient hooks run before the first user input.")

(defvar lightemacs-on-first-file-hook nil
  "Transient hooks run before the first opened file.")

(defvar lightemacs-on-first-buffer-hook nil
  "Transient hooks run before the first visible buffer.")

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

;;; `lightemacs-keyboard-quit'

(defun lightemacs-keyboard-quit ()
  "Execute a context-aware cancel operation.

This command acts as a replacement for the default `keyboard-quit'. It evaluates
the current editor state and applies conditional logic to handle buffer
manipulation, keymap cancellation, and standard interruption operations.

The execution follows this priority:
1. If any minibuffer is open, it aborts the current recursive edit.
2. If the Evil visual state is active, it exits to the normal state.
3. If a region is active, it deactivates the mark.
4. If in a completion or help buffer, it closes that window.
5. Otherwise, it executes the standard `keyboard-quit' command.

:returns: nil
:raises quit: When falling back to the standard `keyboard-quit' function."
  (interactive)
  (cond
   ;; Handle active minibuffer state to ensure state synchronization.
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))

   ;; Handle Evil visual state to ensure state machine synchronization.
   ((and (bound-and-true-p evil-local-mode)
         (fboundp 'evil-visual-state-p)
         (fboundp 'evil-exit-visual-state)
         (evil-visual-state-p))
    (evil-exit-visual-state))

   ;; Handle standard Emacs region/mark.
   ((region-active-p)
    (deactivate-mark))

   ;; Close transient UI windows like Help or Completions.
   ((derived-mode-p 'completion-list-mode 'help-mode)
    (delete-completion-window))

   ;; Fallback to default quit behavior.
   (t
    (keyboard-quit))))

;;; Interactive functions

(defvar auto-insert)

(defun lightemacs--edit-file (filename template)
  "Visit FILENAME, creating its parent directory if necessary.
If FILENAME is new and its buffer is empty, insert TEMPLATE and move point
to the start of the third line, or to the end if it has fewer lines.
Suppress Auto Insert during the visit.  Display the buffer and return it."
  (let ((file (expand-file-name filename)))
    (make-directory (file-name-directory file) t)
    (let ((buffer (let ((auto-insert nil))
                    (find-file-noselect file))))
      (with-current-buffer buffer
        (when (and (zerop (buffer-size))
                   (not (file-exists-p file)))
          (insert template)
          (goto-char (point-min))))
      (switch-to-buffer buffer)
      buffer)))

(defun lightemacs-edit-config ()
  "Open the local Lightemacs configuration file.
Insert a configuration template when creating config.el."
  (interactive)
  (let ((buffer (lightemacs--edit-file
                 (expand-file-name "config.el" lightemacs-local-lisp-directory)
                 (concat ";;; config.el --- Lightemacs configuration "
                         "-*- lexical-binding: t; -*-\n\n"
                         ";;; Commentary:\n"
                         ";; Lightemacs configuration.\n\n"
                         ";;; Code:\n\n"
                         "(setq lightemacs-modules '(le-flavor-big))\n\n"
                         ";;; config.el ends here\n"))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (forward-line 7)))))

(defun lightemacs-edit-local-module (module)
  "Edit a local Lightemacs module named MODULE, with or without its .el suffix.
Offer completion for modules in `lightemacs-local-modules-directory'.
For a new module, insert a template with a matching `provide' form."
  (interactive
   (let* ((directory lightemacs-local-modules-directory)
          (files (when (file-directory-p directory)
                   (directory-files directory nil "\\.el\\'"))))
     (list (completing-read "Edit local module: "
                            (mapcar #'file-name-sans-extension files)))))
  (let ((name (replace-regexp-in-string "\\.el\\'" "" module t t)))
    (unless (string-match-p "\\`[[:alnum:]][[:alnum:]_-]*\\'" name)
      (user-error "Invalid module name: %s" module))
    (let ((filename (concat name ".el")))
      (lightemacs--edit-file
       (expand-file-name filename lightemacs-local-modules-directory)
       (format (concat ";;; %s --- Lightemacs module "
                       "-*- lexical-binding: t; -*-\n\n"
                       "(require 'lightemacs)\n\n"
                       "(provide '%s)\n\n"
                       ";;; %s ends here\n")
               filename name filename)))))

;;; Byte compile

(defun lightemacs--elisp-native-compiled-p (el-file &optional eln-file elc-file)
  "Return non-nil if EL-FILE has a matching native compilation.
EL-FILE is the path to the Emacs Lisp source file.
Check optional ELN-FILE first, then search `native-comp-eln-load-path'
and its `preloaded' subdirectories.  An ordinary .eln file must be at
least as new as EL-FILE and, when supplied, ELC-FILE.  A matching regular
file found in a `preloaded' subdirectory is accepted regardless of its
timestamp."
  (let ((up-to-date-p (lambda (candidate)
                        (and candidate
                             (file-regular-p candidate)
                             (not (file-newer-than-file-p el-file candidate))
                             (or (null elc-file)
                                 (not (file-newer-than-file-p
                                       elc-file candidate)))))))
    (or (funcall up-to-date-p eln-file)
        (and (fboundp 'comp-eln-load-path-eff)
             (when-let* ((eln-basename (if eln-file
                                           (file-name-nondirectory eln-file)
                                         (and (fboundp 'comp-el-to-eln-rel-filename)
                                              (condition-case nil
                                                  (comp-el-to-eln-rel-filename el-file)
                                                (error nil))))))
               (seq-some
                (lambda (preload-dir)
                  (or
                   ;; It checks whether any effective native compilation
                   ;; directory contains either: dir/eln-basename: a regular
                   ;; .eln file newer than el-file.
                   (funcall up-to-date-p (expand-file-name eln-basename preload-dir))
                   ;; dir/preloaded/eln-basename: a matching regular .eln
                   ;; file, regardless of its timestamp.
                   ;;
                   ;; This checks whether the matching .eln file exists as a
                   ;; regular file inside dir/preloaded/.
                   (file-regular-p (expand-file-name
                                    eln-basename
                                    (expand-file-name "preloaded" preload-dir)))))
                (comp-eln-load-path-eff))))
        (and (not eln-file)
             (fboundp 'comp-el-to-eln-filename)
             (funcall up-to-date-p
                      (comp-el-to-eln-filename el-file))))))

(defun lightemacs--byte-compile-file (el-file elc-file)
  "Byte-compile EL-FILE and remove stale ELC-FILE if compilation fails."
  (condition-case err
      (or (byte-compile-file el-file)
          (error "Byte compilation failed"))
    (error
     (when (and (file-regular-p elc-file)
                (file-newer-than-file-p el-file elc-file))
       (delete-file elc-file)
       (lightemacs-debug-message "Deleted stale bytecode: %s" elc-file))
     (signal (car err) (cdr err)))))

(defun lightemacs--native-compile-async (el-file elc-file)
  "Request asynchronous native compilation of EL-FILE for ELC-FILE.
Invalidate an old native output if the worker would otherwise skip it."
  (let ((eln-file (comp-el-to-eln-filename el-file)))
    (when (and (file-regular-p eln-file)
               (file-newer-than-file-p elc-file eln-file)
               (not (file-newer-than-file-p el-file eln-file)))
      (delete-file eln-file))
    (native-compile-async el-file)))

(defun lightemacs-byte-and-native-compile (file &optional any-directory)
  "Compile the Lisp source associated with FILE if compilation is needed.
FILE is the loaded source or .elc filename.  Only compile sources in
allowed directories.  If native compilation is available, request it
after byte compilation succeeds or when current bytecode lacks a matching
native compilation.

Return t if byte compilation succeeds or a native compilation request
returns normally, `no-byte-compile' if the source disables byte
compilation, nil if skipped, and `error' on a synchronous failure.
Errors in a native compiler subprocess occur after this function returns.

When ANY-DIRECTORY is non-nil, FILE is eligible for compilation even if it is
outside `lightemacs-lisp-directory' and all directories listed in
`lightemacs-byte-compile-additional-directories'."
  (condition-case err
      (when (and lightemacs-byte-and-native-compile-after-load
                 (not (bound-and-true-p compile-angel-on-load-mode))
                 file
                 (not (string-suffix-p ".eln" file)))
        (let ((el-file file))
          (when (string-suffix-p ".elc" file)
            (setq el-file (locate-file
                           (substring (file-name-nondirectory file) 0 -1)
                           (list (file-name-directory file))
                           load-file-rep-suffixes
                           #'file-regular-p)))
          (when (and el-file
                     (let ((case-fold-search nil))
                       (string-match-p
                        (concat "\\.el" (regexp-opt load-file-rep-suffixes) "\\'")
                        el-file))
                     (or any-directory
                         (file-in-directory-p el-file lightemacs-lisp-directory)
                         (seq-some
                          (lambda (directory)
                            (file-in-directory-p el-file directory))
                          lightemacs-byte-compile-additional-directories)))
            (let* ((inhibit-message (not (or lightemacs-verbose
                                             lightemacs-debug)))
                   (resolver (if (bound-and-true-p
                                  byte-compile-dest-file-function)
                                 byte-compile-dest-file-function
                               #'byte-compile-dest-file))
                   (elc-file (funcall resolver el-file)))
              (if (file-newer-than-file-p el-file elc-file)
                  ;; .el file is newer
                  (if (not (file-writable-p elc-file))
                      (progn
                        (lightemacs-verbose-message
                          "Skipping byte compilation of %s: cannot write %s"
                          el-file elc-file)
                        nil)
                    (pcase (lightemacs--byte-compile-file el-file elc-file)
                      ('no-byte-compile
                       (lightemacs-debug-message
                         "[IGNORE] Byte-compile: %s" el-file)
                       'no-byte-compile)

                      (_
                       (lightemacs-verbose-message "Byte-compiled: %s" el-file)
                       (when (and (fboundp 'native-comp-available-p)
                                  (native-comp-available-p))
                         (lightemacs--native-compile-async el-file elc-file)
                         (lightemacs-verbose-message
                           "Requested native compilation: %s" el-file))
                       t)))
                ;; .elc file is newer: Check native compilation
                (when (and (fboundp 'native-comp-available-p)
                           (native-comp-available-p)
                           (not (lightemacs--elisp-native-compiled-p
                                 el-file nil elc-file)))
                  (lightemacs--native-compile-async el-file elc-file)
                  (lightemacs-verbose-message
                    "Requested native compilation: %s" el-file)
                  t))))))
    (error
     (lightemacs-verbose-message
       "[ERROR] Could not compile %s: %s" file (error-message-string err))
     'error)))

;;; Lightemacs modules

(defmacro lightemacs-module-hooks (module-name mode-func target-hooks)
  "Register hooks where MODE-FUNC should be enabled for MODULE-NAME.

MODULE-NAME is the module symbol, such as `flycheck'.
MODE-FUNC is the function symbol, such as `flycheck-mode'.
TARGET-HOOKS is a form evaluating to a hook symbol or a list of hooks.

Declare `lightemacs-MODULE-NAME-target-hooks' unless it is already
globally special.  On each call, set its value to TARGET-HOOKS and
register MODE-FUNC on each specified hook."
  (declare (indent 1) (debug (symbolp symbolp form)))
  (let ((target-var (intern (format "lightemacs-%s-target-hooks" module-name)))
        (docstring (format "List of hooks where `%s' is enabled." mode-func)))
    `(progn
       (defvar ,target-var ,target-hooks ,docstring)
       (dolist (target-hook (ensure-list ,target-var))
         (add-hook target-hook #',mode-func)))))

(defmacro lightemacs-module-bind (module &rest body)
  "Evaluate BODY to install key bindings for MODULE.

If `lightemacs-MODULE-inhibit-keybindings' is non-nil, skip BODY."
  (declare (indent 1) (debug (sexp body)))
  (let ((inhibit-var (intern (format "lightemacs-%s-inhibit-keybindings"
                                     module))))
    `(progn
       (defvar ,inhibit-var nil
         ,(format "When non-nil, skip default key bindings for `%s'."
                  module))
       (unless (bound-and-true-p ,inhibit-var)
         ,@body))))

(defmacro lightemacs-module-setq-maybe (name &rest args)
  "Apply the variable-value pairs in ARGS as defaults for module NAME.

If `lightemacs-NAME-inhibit-defaults' is non-nil, skip all assignments
without evaluating their value expressions."
  (declare (indent 1) (debug t))
  (let ((inhibit-var (intern (format "lightemacs-%s-inhibit-defaults" name)))
        (pairs args)
        targets)
    (while pairs
      (push (car pairs) targets)
      (setq pairs (cddr pairs)))
    `(progn
       (defvar ,inhibit-var nil
         ,(format "When non-nil, skip default settings for `%s'." name))
       ;; Runtime: Single optimized check
       (unless (bound-and-true-p ,inhibit-var)
         (with-suppressed-warnings ((free-vars ,@targets))
           (setq ,@args))))))

(defmacro lightemacs-module-defaults (name &rest body)
  "Evaluate BODY to configure defaults for module NAME.

NAME is an unquoted symbol or a string.
If `lightemacs-NAME-inhibit-defaults' is non-nil, skip BODY."
  (declare (indent 1) (debug (sexp body)))
  (let ((inhibit-var (intern (format "lightemacs-%s-inhibit-defaults" name))))
    `(progn
       (unless (special-variable-p ',inhibit-var)
         (defvar ,inhibit-var nil
           ,(format "When non-nil, skip default settings for `%s'." name)))
       ;; Runtime: Single optimized check
       (unless (bound-and-true-p ,inhibit-var)
         ,@body))))

(defun lightemacs-module-load (modules)
  "Load the features in MODULES in order using `load-path'."
  (dolist (feature-symbol modules)
    (lightemacs-verbose-message "Load module: %s" feature-symbol)
    (require feature-symbol)))

;; TODO Remove
;; (defun lightemacs-module-load (modules)
;;   "Load the features in MODULES in order.
;; Search the bundled module directory before the local module directory.
;; Propagate any error raised while loading a module."
;;   (let ((priority-path (cons lightemacs-builtin-modules-directory
;;                              (cons lightemacs-local-modules-directory
;;                                    load-path))))
;;     (dolist (feature-symbol modules)
;;       (lightemacs-verbose-message "Load module: %s" feature-symbol)
;;       (unless (featurep feature-symbol)
;;         (let ((filename (locate-library (symbol-name feature-symbol)
;;                                         nil priority-path)))
;;           (unless filename
;;             (error "Cannot find module '%s'" feature-symbol))
;;           (require feature-symbol
;;                    (lightemacs--remove-el-file-suffix filename)))))))

;;; Core CLI tools

(defvar lightemacs--ripgrep-executable nil)
(defvar lightemacs--fdfind-executable nil)

(defun lightemacs--load-cli-tools ()
  "Find cli tools."
  (unless lightemacs--ripgrep-executable
    (setq lightemacs--ripgrep-executable (executable-find "rg")))

  (unless lightemacs--fdfind-executable
    (setq lightemacs--fdfind-executable
          (let ((fdfind-executable (executable-find "fd")))
            (if fdfind-executable
                fdfind-executable
              ;; Debian
              (executable-find "fdfind"))))))

;;; Core process buffers

(defvar lightemacs-process-buffers-modes
  '(vterm-mode
    eat-mode
    term-mode
    ghostel-mode
    compilation-mode)
  "List of terminal emulator major modes.")

(defvar lightemacs-process-buffers-compilation-modes
  '(compilation-mode)
  "List of compilation modes.")

(defvar lightemacs-process-buffers-compilation-modes-never-disable
  '(;; Build logs often contain long file paths, compiler flags, linker
    ;; commands, and stack traces. Keeping visual-line-mode active wraps long
    ;; lines within the window so output stays readable without forcing
    ;; horizontal scrolling or cutting off text at the right edge.
    visual-line-mode

    ;; Highlighting the current line helps you track which diagnostic, warning,
    ;; or error line you are inspecting as you step through build output or jump
    ;; between errors.
    hl-line-mode)
  "List of minor modes to keep enabled in compilation buffers.
These modes are exempt from being disabled in buffers matching
`lightemacs-process-buffers-compilation-modes'.")

(defvar lightemacs-process-buffers-disabled-modes
  '(;; Highlights the current line, which is generally unnecessary in a
    ;; terminal buffer and can alter the appearance of terminal output.
    ;; NOTE: This was disabled by le-default-settings
    ;; hl-line-mode

    ;; Displays line numbers in the window margin, which are not part of the
    ;; terminal's character grid.
    display-line-numbers-mode

    ;; Draws a vertical indicator in the window, which is unrelated to the
    ;; terminal's character grid.
    ;; NOTE: This was disabled by le-default-settings
    ;; display-fill-column-indicator-mode

    ;; Enables Emacs visual-line wrapping rather than the terminal emulator's
    ;; own line wrapping.
    visual-line-mode

    ;; Displays whitespace characters and related visual indicators, which are
    ;; generally unnecessary in terminal buffers and can alter their appearance.
    ;; NOTE: This was disabled by le-default-settings
    ;; whitespace-mode

    ;; Adds faces to matching delimiters, which is generally unnecessary in
    ;; terminal buffers and can alter terminal output's appearance.
    rainbow-delimiters-mode
    ;; Requests contextual documentation, which is generally not useful in
    ;; terminal emulator buffers.
    eldoc-mode
    ;; These modes perform Emacs editing actions such as inserting matching
    ;; delimiters or indentation. Such behavior is generally undesirable when
    ;; terminal input should be handled by the terminal application.
    electric-pair-local-mode
    electric-indent-local-mode
    show-paren-local-mode
    smartparens-mode
    ;; These packages provide Emacs-side completion or expansion features that
    ;; are generally unnecessary when completion is handled by the terminal
    ;; application or shell.
    yas-minor-mode
    yas/minor-mode
    company-mode
    ;; NOTE: This was disabled by le-corfu
    ;; corfu-mode
    )
  "List of minor modes to disable in terminal emulator buffers.")

(defvar lightemacs-process-buffers-disabled-global-modes
  '(;; Highlights the current line, which is generally unnecessary in a
    ;; terminal buffer and can alter the appearance of terminal output.
    ;; NOTE: This was disabled by le-default-settings
    ;; global-hl-line-mode

    ;; Displays line numbers in the window margin, which are not part of the
    ;; terminal's character grid.
    global-display-line-numbers-mode

    ;; Draws a vertical indicator in the window, which is unrelated to the
    ;; terminal's character grid.
    ;; NOTE: This was disabled by le-default-settings
    ;; global-display-fill-column-indicator-mode

    ;; Enables Emacs visual-line wrapping rather than the terminal emulator's
    ;; own line wrapping.
    global-visual-line-mode

    ;; Displays whitespace characters and related visual indicators, which are
    ;; generally unnecessary in terminal buffers and can alter their appearance.
    ;; NOTE: This was disabled by le-default-settings
    ;; global-whitespace-mode

    ;; These modes perform Emacs editing actions such as inserting matching
    ;; delimiters or indentation. Such behavior is generally undesirable when
    ;; terminal input should be handled by the terminal application.
    electric-pair-mode
    electric-indent-mode
    show-paren-mode
    smartparens-global-mode
    ;; These packages provide Emacs-side completion or expansion features that
    ;; are generally unnecessary when completion is handled by the terminal
    ;; application or shell.
    yas-global-mode
    yas/global-mode
    global-company-mode
    ;; NOTE: This was disabled by le-corfu
    ;; global-corfu-mode
    )
  "List of global minor modes to disable locally in terminal emulator buffers.

Disabling a local minor mode directly is often insufficient because globalized
minor modes may reactivate their local counterparts via hooks during buffer
initialization. Listing the global mode variables here ensures the terminal
setup function can make them buffer-local and set them to nil, which prevents
the global modes from overriding the local configuration.

This prevents, for example, `global-hl-line-mode' from automatically
reactivating the local mode via hooks. Setting the global minor mode variable to
nil locally acts as a shield for this specific buffer.")

(defun lightemacs--lightemacs-process-buffers-optimize ()
  "Configure terminal and compilation buffers."
  (let ((eat-p (derived-mode-p 'eat-mode))
        (vterm-p (derived-mode-p 'vterm-mode))
        (term-p (derived-mode-p 'term-mode))
        (comp-p (apply #'derived-mode-p lightemacs-process-buffers-compilation-modes))
        (inhibit-redisplay t)
        (inhibit-message t))
    (when eat-p
      (setq-local font-lock-defaults '(nil t)))

    (when (or term-p eat-p)
      ;; Stops long lines from wrapping
      (setq-local truncate-lines t)
      ;; Setting this variable to `most-positive-fixnum' makes Emacs scrolls
      ;; only enough to bring point into view rather than recentering.
      (setq-local scroll-conservatively most-positive-fixnum))

    (when (or term-p eat-p vterm-p)
      (setq-local line-spacing 0))

    (unless comp-p
      (setq-local auto-hscroll-mode nil))

    (when comp-p
      (visual-line-mode 1))

    (setq-local nobreak-char-display nil)

    (dolist (mode-fn lightemacs-process-buffers-disabled-modes)
      (unless (and comp-p
                   (memq mode-fn lightemacs-process-buffers-compilation-modes-never-disable))
        (when (and (boundp mode-fn)
                   (fboundp mode-fn))
          ;; Using (symbol-value mode) doesn't work. What works is to
          ;; unconditionally disable the mode if the function exists. Explicitly
          ;; calling (funcall mode -1) is the standard practice, as it sets a
          ;; local state that causes globalized modes not to reactivate in that
          ;; specific buffer.
          (ignore-errors
            (funcall mode-fn -1)))))

    ;; Prevent globalized minor modes from reactivating the mode by setting
    ;; their global variable to nil locally. This fixes modes such as
    ;; `global-hl-line-mode' that activate anyway.
    (dolist (global-var lightemacs-process-buffers-disabled-global-modes)
      (when (boundp global-var)
        (set (make-local-variable global-var) nil)))))

(defun lightemacs--process-buffers-disable-process-query-on-exit ()
  "Disable the query-on-exit flag for the process in the current buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-query-on-exit-flag proc nil))))

(defun lightemacs--process-buffers-disable-kill-prompt ()
  "Prevent Emacs from prompting: Buffer has a running process; kill it?"
  ;; Prevent Emacs from prompting "Buffer has a running process; kill it?" when
  ;; closing the buffer or exiting the editor by silently disabling the
  ;; query-on-exit flag for the underlying shell process.
  (cond
   ;; Vterm creates the process during mode init, so it exists now.
   ((derived-mode-p 'vterm-mode)
    (lightemacs--process-buffers-disable-process-query-on-exit))
   ;; Term create the process AFTER the mode hook runs. We must defer
   ;; the flag change using their respective exec hooks.
   ((derived-mode-p 'term-mode)
    (add-hook 'term-exec-hook #'lightemacs--process-buffers-disable-process-query-on-exit nil t))))

;;; Provide lightemacs

(provide 'lightemacs)

;;; lightemacs.el ends here
