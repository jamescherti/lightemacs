;;; le-keybindings.el --- le-keybindings -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Custom keybindings

;;; Code:

(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)

(provide 'le-keybindings)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-keybindings.el ends here
