;;; le-xclip.el --- le-xclip -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The le-xclip module integrates the xclip package, enabling copy and paste
;; between Emacs running in a terminal and the system GUI clipboard.
;;
;; IMPORTANT: This module only activates the xclip package when Emacs is
;; executed in a terminal.
;;
;; The xclip package relies on external command-line tools depending on the
;; platform: xclip or xsel for X11, pbpaste/pbcopy for macOS, getclip/putclip
;; for Cygwin, wl-clipboard for Wayland, and
;; termux-clipboard-get/termux-clipboard-set for Termux. Emacs with GUI support
;; can also access the clipboard directly.
;;
;; URL: https://elpa.gnu.org/packages/xclip.html

;;; Code:

(eval-and-compile
  (require 'le-core-package-manager))

(use-package xclip
  :if (not (display-graphic-p))
  :commands xclip-mode
  :hook (after-init . xclip-mode))

(provide 'le-xclip)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-xclip.el ends here
