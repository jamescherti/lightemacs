;;; le-persist-text-scale.el --- le-persist-text-scale -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The persist-text-scale Emacs package provides `persist-text-scale-mode',
;; which ensures that all adjustments made with `text-scale-increase' and
;; `text-scale-decrease' are persisted and restored across sessions. As a
;; result, the text size in each buffer remains consistent, even after
;; restarting Emacs.
;;
;; This package also facilitates grouping buffers into categories, allowing
;; buffers within the same category to share a consistent text scale. This
;; ensures uniform font sizes when adjusting text scaling. By default:
;; - Each file-visiting buffer has its own independent text scale.
;; - Special buffers, identified by their buffer names, each retain their own
;;   text scale setting.
;; - All Dired buffers maintain the same font size, treating Dired as a unified
;;   "file explorer" where the text scale remains consistent across different
;;   buffers.
;;
;; This category-based behavior can be further customized by assigning a
;; function to the `persist-text-scale-buffer-category-function' variable. The
;; function determines how buffers are categorized by returning a category
;; identifier (string) based on the buffer's context. Buffers within the same
;; category will share the same text scale.
;;
;; URL: https://github.com/renzmann/persist-text-scale

;;; Code:

(eval-and-compile
  (require 'lightemacs)
  (require 'le-diminish))

(lightemacs-use-package
  persist-text-scale
  :diminish persist-text-scale-mode
  :commands (persist-text-scale-mode
             persist-text-scale-restore)

  :hook (after-init . persist-text-scale-mode)

  :init
  (setq text-scale-mode-step 1.07))

(provide 'le-persist-text-scale)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-persist-text-scale.el ends here
