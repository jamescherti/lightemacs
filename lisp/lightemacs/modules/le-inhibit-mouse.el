;;; le-inhibit-mouse.el --- le-inhibit-mouse -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Disable mouse mouse input in Emacs.
;;
;; URL: https://github.com/jamescherti/inhibit-mouse.el

;;; Code:

(require 'lightemacs-use-package)

(lightemacs-use-package inhibit-mouse
  :commands inhibit-mouse-mode
  :init
  (setq inhibit-mouse-adjust-show-help-function t)
  (setq inhibit-mouse-adjust-mouse-highlight t)

  ;; TODO Add this to inhibit-mouse
  (add-hook 'inhibit-mouse-mode-hook
            #'(lambda()
                ;; Enable or disable `tooltip-mode'. When tooltip-mode is
                ;; enabled, certain UI elements (e.g., help text, mouse-hover
                ;; hints) will appear as native system tooltips (pop-up
                ;; windows), rather than as echo area messages. This is useful
                ;; in graphical Emacs sessions where tooltips can appear near
                ;; the cursor.
                (when (fboundp 'tooltip-mode)
                  (if (bound-and-true-p inhibit-mouse-mode)
                      (tooltip-mode -1)
                    (tooltip-mode 1)))))

  ;; TODO Add this to inhibit-mouse
  (add-hook 'inhibit-mouse-mode-hook
            #'(lambda()
                ;; Enable or disable the context menu based on the state of
                ;; `inhibit-mouse-mode', the following code dynamically toggles
                ;; `context-menu-mode' accordingly.
                (when (fboundp 'context-menu-mode)
                  (if (bound-and-true-p inhibit-mouse-mode)
                      (context-menu-mode -1)
                    (context-menu-mode 1)))))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'inhibit-mouse-mode)
    (add-hook 'lightemacs-after-init-hook #'inhibit-mouse-mode)))

(provide 'le-inhibit-mouse)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-inhibit-mouse.el ends here
