;;; org-structtempl.el --- Get good old structure-templates back into org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  DREWOR020

;; Author: Tobias Zawada <i@tn-home.de>
;; Keywords: wp, convenience
;; Version: 1.0.0
;; URL: https://github.com/TobiasZawada/org-structtempl
;; Package-Requires: ((emacs "26.3") (org "9.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a slightly modified copy of the org structure template code
;; from `org.el' of org-mode version 9.1.14.
;; The functions herein have been given a unique prefix `org-structtempl'.
;; Furthermore, the names have been shortened a bit.
;; Each function has a leading comment with the original function name.

;;; Code:
(require 'org)

;; Original: `org-structure-template-alist'
(defcustom org-structtempl-alist
  '(("s" "#+BEGIN_SRC ?\n\n#+END_SRC")
    ("e" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
    ("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE")
    ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE")
    ("V" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM")
    ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER")
    ("C" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
    ("l" "#+BEGIN_EXPORT latex\n?\n#+END_EXPORT")
    ("L" "#+LaTeX: ")
    ("h" "#+BEGIN_EXPORT html\n?\n#+END_EXPORT")
    ("H" "#+HTML: ")
    ("a" "#+BEGIN_EXPORT ascii\n?\n#+END_EXPORT")
    ("A" "#+ASCII: ")
    ("i" "#+INDEX: ?")
    ("I" "#+INCLUDE: %file ?"))
  "Structure completion elements.
This is a list of abbreviation keys and values.  The value gets inserted
if you type `<' followed by the key and then press the completion key,
usually `TAB'.  %file will be replaced by a file name after prompting
for the file using completion.  The cursor will be placed at the position
of the `?' in the template.
There are two templates for each key, the first uses the original Org syntax,
the second uses Emacs Muse-like syntax tags.  These Muse-like tags become
the default when the /org-mtags.el/ module has been loaded.  See also the
variable `org-mtags-prefer-muse-templates'."
  :group 'org-edit-structure
  :type '(repeat
	  (list
	   (string :tag "Key")
	   (string :tag "Template"))))

;; Original: `org-try-structure-completion'
(defun org-structtempl-try-completion ()
  "Try to complete a structure template before point.
This looks for strings like \"<e\" on an otherwise empty line and
expands them."
  (let ((l (buffer-substring (point-at-bol) (point)))
	a)
    (when (and (looking-at "[ \t]*$")
	       (string-match "^[ \t]*<\\([a-zA-Z]+\\)$" l)
	       (setq a (assoc (match-string 1 l) org-structtempl-alist)))
      (org-structtempl-expand (+ -1 (point-at-bol)
				 (match-beginning 1))
			      a)
      t)))

;; Original: `org-complete-expand-structure-template'
(defun org-structtempl-expand (start cell)
  "Expand a structure template at START.
CELL is the matching entry in `org-structtempl-alist'."
  (let ((rpl (nth 1 cell))
	(ind ""))
    (delete-region start (point))
    (when (string-match "\\`[ \t]*#\\+" rpl)
      (cond
       ((bolp))
       ((not (string-match "\\S-" (buffer-substring (point-at-bol) (point))))
	(setq ind (buffer-substring (point-at-bol) (point))))
       (t (newline))))
    (setq start (point))
    (when (string-match "%file" rpl)
      (setq rpl (replace-match
		 (concat
		  "\""
		  (save-match-data
		    (abbreviate-file-name (read-file-name "Include file: ")))
		  "\"")
		 t t rpl)))
    (setq rpl (mapconcat 'identity (split-string rpl "\n")
			 (concat "\n" ind)))
    (insert rpl)
    (when (re-search-backward "\\?" start t) (delete-char 1))))

(add-hook 'org-tab-after-check-for-cycling-hook #'org-structtempl-try-completion)

(provide 'org-structtempl)
;;; org-structtempl.el ends here
