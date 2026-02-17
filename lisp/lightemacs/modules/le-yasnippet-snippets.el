;;; le-yasnippet-snippets.el --- le-yasnippet-snippets -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The yasnippet-snippets package with a comprehensive collection of bundled
;; templates for numerous programming and markup languages, including C, C++,
;; C#, Perl, Python, Ruby, SQL, LaTeX, HTML, CSS...

;; URL: https://github.com/AndreaCrotti/yasnippet-snippets

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package)
  (require 'le-yasnippet))

(lightemacs-use-package yasnippet-snippets
  :after yasnippet)

(provide 'le-yasnippet-snippets)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; le-yasnippet-snippets.el ends here
