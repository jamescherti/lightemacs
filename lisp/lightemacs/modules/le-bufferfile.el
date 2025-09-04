;;; le-bufferfile.el --- le-bufferfile -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides helper functions to delete, rename, or copy buffer
;; files:
;;
;; - `bufferfile-rename': Renames the file visited by the current buffer,
;;   ensures that the destination directory exists, and updates the buffer name
;;   for all associated buffers, including clones/indirect buffers. It also
;;   ensures that buffer-local features referencing the file, such as Eglot or
;;   `dired' buffers, are correctly updated to reflect the new file name.
;; - `bufferfile-delete': Delete the file associated with a buffer and kill all
;;   buffers visiting the file, including clones/indirect buffers.
;; - `bufferfile-copy': Ensures that the destination directory exists and copies
;;   the file visited by the current buffer to a new file.
;;
;; The functions above also ensures that any modified buffers are saved prior to
;; executing operations like renaming, deleting, or copying.
;;
;; (To make bufferfile use version control when renaming or deleting files, you
;; can set the variable bufferfile-use-vc to t. This ensures that file
;; operations within bufferfile interact with the version control system,
;; preserving history and tracking changes properly.)
;;
;; URL: https://github.com/jamescherti/bufferfile.el

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package bufferfile
  :commands (bufferfile-rename
             bufferfile-delete
             bufferfile-copy
             bufferfile-dired-do-rename))

(provide 'le-bufferfile)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-bufferfile.el ends here
