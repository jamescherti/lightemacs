;;; le-ace-window.el --- le-ace-window -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ace Window provides a fast and efficient method for switching between windows
;; in a frame. Instead of cycling through windows sequentially or using more
;; cumbersome key sequences, Ace Window displays a single-letter label on each
;; visible window, allowing the user to jump directly to a target window by
;; pressing the corresponding key.
;;
;; URL: https://github.com/abo-abo/ace-window

;;; Code:

(eval-and-compile
  (require 'lightemacs))

(lightemacs-use-package
  ace-window
  :commands ace-window
  :bind
  ([remap other-window] . ace-window)
  :custom
  (aw-background nil)  ; t is not compatible with all themes
  (aw-scope 'frame))

(provide 'le-ace-window)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; le-ace-window.el ends here
