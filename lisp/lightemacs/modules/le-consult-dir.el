;;; le-consult-dir.el --- le-consult-dir -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The consult-dir package extends the Consult framework by providing convenient
;; ways to quickly switch to and insert directory paths.
;;
;; Similar to shell utilities such as autojump, fasd, or z, `consult-dir'
;; provides directory-jumping capabilities within Emacs. The package integrates
;; with all Emacs commands that require file path input, as well as with Embark
;; actions on files.
;;
;; Candidate directories are collected from user bookmarks, Projectile project
;; roots (if available), project roots (if available), and locations of files
;; tracked by `recentf'. Additionally, if 'fasd' is installed, this module
;; incorporates directory candidates from it as well.
;;
;; URL: https://github.com/karthink/consult-dir

;;; Code:

(eval-and-compile
  (require 'lightemacs))

;; Variables

(defvar lightemacs-consult-dir-fasd-support t
  "If non-nil, enable integration of fasd with `consult-dir'.
When enabled and fasd is available in PATH, directory candidates from fasd will
be added as a source for `consult-dir'. If fasd is not installed or not found in
PATH, this option has no effect.")

;;; Use-package

(lightemacs-use-package consult-dir
  :commands (consult-dir
             consult-dir-jump-file
             consult-dir--pick
             consult-dir-default-command)
  :functions consult-dir--fasd-dirs

  :bind (("C-x C-d" . lightemacs-consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . lightemacs-consult-dir)
         ("C-x C-j" . consult-dir-jump-file))

  :preface
  ;; Fixes: https://github.com/karthink/consult-dir/issues/43
  ;; Issues fixed: Typing Alt + Shift + :edit and then pressing C-x C-d to
  ;; invoke consult-dir causes the selected directory to be inserted before
  ;; :edit, rather than at the cursor position after :edit.
  (defun lightemacs-consult-dir ()
    "Choose a directory and act on it.

The action taken on the directory is the value of `consult-dir-default-command'.
The default is to call `find-file' starting at this directory.

When called from the minibuffer, insert the directory into the minibuffer prompt
instead. Existing minibuffer contents will be shadowed or deleted depending on
the value of `consult-dir-shadow-filenames'.

The list of sources for directory paths is `consult-dir-sources', which can be
customized."
    (interactive)
    (if (not (minibufferp))
        (consult-dir)
      ;; Minibuffer
      ;; Fix this bug: https://github.com/karthink/consult-dir/issues/43
      (let* ((enable-recursive-minibuffers t)
             (new-dir (consult-dir--pick))
             (new-full-name (file-name-as-directory new-dir)))
        (when new-dir
          (if consult-dir-shadow-filenames
              (insert "/" new-full-name)
            (insert new-full-name))))))

  :init
  ;; Disable prepending of `/` to paths by consult-dir
  ;;
  ;; The purpose of prepending paths with / when consult-dir-shadow-filenames
  ;; set to t is unclear.
  ;; Issue: https://github.com/karthink/consult-dir/issues/44
  (setq consult-dir-shadow-filenames nil)

  :config
  ;; FASD
  (when (executable-find "fasd")
    ;; A function that returns a list of directories
    (defun consult-dir--fasd-dirs ()
      "Return list of fasd dirs."
      (split-string (shell-command-to-string "fasd -ld") "\n" t))

    ;; A consult source that calls this function
    (defvar consult-dir--source-fasd
      `(:name     "Fasd dirs"
                  :narrow   ?f
                  :category file
                  :face     consult-file
                  :history  file-name-history
                  :enabled  ,(lambda () (executable-find "fasd"))
                  :items    ,#'consult-dir--fasd-dirs)
      "Fasd directory source for `consult-dir'.")

    ;; Adding to the list of consult-dir sources
    (add-to-list 'consult-dir-sources 'consult-dir--source-fasd t)))

(provide 'le-consult-dir)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-consult-dir.el ends here
