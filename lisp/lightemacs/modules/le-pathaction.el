;;; le-pathaction.el --- le-pathaction -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configures pathaction.el, en Emacs package that integrates the pathaction
;; CLI, allowing execution of .pathaction.yaml rules directly from Emacs.
;;
;; Pathaction acts as a universal Makefile for all files in the filesystem,
;; mapping files or directories to commands defined in YAML with optional Jinja2
;; templating and tags for multiple actions per file type. The tool targets
;; developers working across many projects and ecosystems and provides a single
;; consistent command to run the appropriate action for any file.
;;
;; URL: https://github.com/jamescherti/pathaction.el

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package pathaction
  :commands (pathaction-edit
             pathaction-run))

(provide 'le-pathaction)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-pathaction.el ends here
