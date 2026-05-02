;;; le-quick-sdcv.el --- le-quick-sdcv -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The quick-sdcv package as a lightweight interface for the sdcv command-line
;; tool. This enables Emacs to function as an offline dictionary, allowing
;; searching for words and translations directly within the editor using local
;; dictionary files.
;;
;; URL: https://github.com/jamescherti/quick-sdcv.el

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package quick-sdcv
  :commands (quick-sdcv-search-at-point
             quick-sdcv-search-input)

  :init
  (setq quick-sdcv-ellipsis lightemacs-ellipsis)
  (setq quick-sdcv-unique-buffers t))

(provide 'le-quick-sdcv)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-quick-sdcv.el ends here
