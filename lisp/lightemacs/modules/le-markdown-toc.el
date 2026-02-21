;;; le-markdown-toc.el --- le-markdown-toc -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Automatically generate or refresh the table of contents in Markdown files
;; using the 'markdown-toc-generate-or-refresh-toc' function.
;; URL: https://github.com/ardumont/markdown-toc

;;; Code:

(require 'lightemacs-module)

(lightemacs-use-package markdown-toc
  :commands (markdown-toc-generate-toc
             markdown-toc-generate-or-refresh-toc
             markdown-toc-delete-toc
             markdown-toc--toc-already-present-p)

  :preface
  (defun le-markdown-toc--markdown-toc-generate-toc-advice (fn &rest args)
    "Restore `window-start' after generating a table of contents.
FN is the advised function. ARGS are the function arguments."
    (lightemacs-save-window-start
      (lightemacs-save-window-hscroll
        (save-excursion
          (apply fn args)))))

  :config
  (advice-add 'markdown-toc-generate-toc :around
              #'le-markdown-toc--markdown-toc-generate-toc-advice))

(provide 'le-markdown-toc)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-markdown-toc.el ends here
