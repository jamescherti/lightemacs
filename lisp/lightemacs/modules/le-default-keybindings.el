;;; le-default-keybindings.el --- le-default-keybindings -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Custom keybindings

;;; Code:

(require 'lightemacs) ; `lightemacs-keyboard-quit'

;;; Keymap override

;; This keymap override is necessary because Emacs resolves keybindings through
;; a strict and often competitive hierarchy.
;;
;; Standard global bindings are easily shadowed by major modes, minor modes, or
;; keymap managers like Evil mode. By injecting our custom keymap directly into
;; emulation-mode-map-alists, we place these specific bindings at the absolute
;; highest precedence level possible within the Emacs input system.
;;
;; This ensures that core shortcuts, such as text scaling, will always execute
;; reliably regardless of the current buffer's mode or the active Evil state. It
;; prevents the frustration of having newly installed packages silently hijack
;; your preferred shortcuts, ensuring a consistently predictable editing
;; experience.

;; Define a dummy variable that is always true so the map stays active
(defvar lightemacs-keymap-override-mode t
  "Always non-nil to ensure the override keymap remains active.")

(defvar lightemacs-keymap-override-map (make-sparse-keymap)
  "Keymap with highest precedence for text scaling.")

;; Create the alist required by emulation-mode-map-alists
(defvar lightemacs--keymap-override-alist
  `((lightemacs-keymap-override-mode . ,lightemacs-keymap-override-map))
  "Alist to be added to `emulation-mode-map-alists'.")

(unless noninteractive
  (dolist (binding '(("C--" . text-scale-decrease)
                     ("C-+" . text-scale-increase)
                     ("C-0" . text-scale-adjust)))
    (let ((key (car binding))
          (cmd (cdr binding)))
      (if (fboundp 'keymap-set)
          (keymap-set lightemacs-keymap-override-map key cmd)
        (define-key lightemacs-keymap-override-map (kbd key) cmd))))

  ;; Insert the alist into the core Emacs input resolution system
  (add-to-list 'emulation-mode-map-alists 'lightemacs--keymap-override-alist))

(provide 'le-default-keybindings)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-default-keybindings.el ends here
