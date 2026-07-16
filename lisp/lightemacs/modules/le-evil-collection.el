;;; le-evil-collection.el --- le-evil-collection -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Evil is an extensible vi layer for Emacs. It emulates the main features of
;; Vim, and provides facilities for writing custom extensions.
;;
;; This configures the evil-collection package.
;;
;; URL: https://github.com/emacs-evil/evil-collection

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(eval-and-compile
  ;; This has to be defined before evil
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-collection-setup-minibuffer t))

(require 'le-evil)

(defvar lightemacs-evil-collection-inhibit-unimpaired-mode nil
  "Inhibit `global-evil-collection-unimpaired-mode' when non-nil.

By default, the `evil-collection' package automatically activates unimpaired
bindings globally. While these Vim-like bracket shortcuts (e.g., [b and ]b to
cycle buffers) are convenient, they aggressively hijack the [ and ] prefixes
across the entire editor.

Setting this variable to t deactivates these global overrides after
initialization, allowing users to retain standard major-mode behaviors unless
they explicitly opt out of the inhibition.")

(lightemacs-use-package evil-collection
  :if (not noninteractive)
  :functions (evil-collection-init
              global-evil-collection-unimpaired-mode)
  :after evil
  :init
  ;; By default, when the completion menu is active, Evil's insert state or the
  ;; minibuffer's standard behavior intercepts the Enter key to execute the
  ;; command or create a newline.
  ;;
  ;; The added configuration explicitly binds RET to corfu-insert within the
  ;; corfu-map. This ensures that when the completion popup is visible,
  ;; pressing Enter selects the highlighted candidate instead of exiting the
  ;; minibuffer or inserting a literal newline.
  (setq evil-collection-corfu-key-themes '(default magic-return))

  :init
  ;; Corfu: Fix magic-return issue in GUI mode
  ;; Issue report: corfu: Add "<return>" to corfu-map when magic-return is enabled
  ;; URL: https://github.com/emacs-evil/evil-collection/pull/895
  (with-eval-after-load 'corfu
    (when (memq 'magic-return evil-collection-corfu-key-themes)
      (evil-define-key 'insert corfu-map (kbd "<return>") 'corfu-insert)))

  (evil-collection-init)

  ;; Disable unimpaired mappings globally
  (when lightemacs-evil-collection-inhibit-unimpaired-mode
    (when (bound-and-true-p global-evil-collection-unimpaired-mode)
      (global-evil-collection-unimpaired-mode -1))))

(provide 'le-evil-collection)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; le-evil-collection.el ends here
