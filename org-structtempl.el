;;; org-structtempl.el --- Get good old structure-templates back into org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

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

;; This is a modified copy of the org structure template code
;; from `org.el' of org-mode version 9.1.14.
;; The functions herein have been given a unique prefix `org-structtempl'.
;; Furthermore, the names have been shortened a bit.
;; Each function has a leading comment with the original function name.

;;; Code:
(require 'org)
(eval-when-compile
  (require 'cl-lib))

;; Original: `org-structure-template-alist'
;;;###autoload
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
    ("I" "#+INCLUDE: %file ?")
    ("m" org-structure-template-latex))
  "Structure completion elements.
This is a list of abbreviation keys and values.  The value gets inserted
if you type `<' followed by the key and then press the completion key,
usually `TAB'.  %file will be replaced by a file name after prompting
for the file using completion.  The cursor will be placed at the position
of the `?' in the template."
  :group 'org-edit-structure
  :type '(repeat
	  (list
	   (string :tag "Key")
	   (choice :tag "Template Source"
	    (string :tag "Template String")
	    (function :tag "Function")))))

;; Corrected version of `org-structure-template-at-point' from org+-structtempl.el:
(defun org-structtempl-at-point (&optional point)
  "Return structure template type at POINT and move point behind it.
Return nil and do not modify point if there is no structure template at point."
  (let (template)
    (save-excursion
      (when point
	(goto-char point)
	(setq point nil))
      (forward-line 0)
      (when (looking-at
	     (concat "[ \t]*<"
		     (regexp-opt
		      (mapcar #'car org-structtempl-alist)
		      'words)))
	(setq point (match-end 1)
	      template (match-string-no-properties 1))))
    (when point
      (goto-char point))
    template))
;; Test: (assert (equal (with-temp-buffer (insert "blah\n\n\t <s something\nother") (goto-char 8) (list (org-structtempl-at-point) (buffer-substring (point) (line-end-position)))) '("s" " something")))

;; Original: `org-try-structure-completion'
;;;###autoload
(defun org-structtempl-try-completion ()
  "Try to complete a structure template before point.
This looks for lines starting with indentation and
a string like \"<e \".
The rest of the line is retained at the end of the
first line in the expansion."
  (let (a)
    (when (and (region-active-p)
	       (< (mark) (point)))
      (exchange-point-and-mark))
    (when (setq a (org-structtempl-at-point))
      (org-structtempl-expand (assoc-string a org-structtempl-alist))
      )))

(defun org-structtempl-buffer-substring-delete (b e)
  "Delete region from B to E and return it as string."
  (if (< b e)
      (prog1
	  (buffer-substring b e)
	(delete-region b e))
    ""))

;; Original: `org-complete-expand-structure-template'
(defun org-structtempl-expand (cell)
  "Expand a structure template at current line.
Point is behind structure template marker.
The rest of the current line counts as sequence of header args.
CELL is the matching entry in `org-structtempl-alist'."
  (let* (start
	 (header (buffer-substring-no-properties (point) (line-end-position)))
	 (rpl (nth 1 cell))
	 (ind (current-indentation))
	 (content (or
		   (and (use-region-p)
			(cl-find ?? rpl)
			(org-structtempl-buffer-substring-delete
			 (save-excursion
			   (goto-char (region-beginning))
			   (line-beginning-position 2))
			 (region-end)))
		   ""))
	 (end-marker (make-marker))
	 (pt-marker (make-marker)))
    ;; Support for function symbols in `org-structtempl-alist':
    (when (functionp rpl)
      (setq rpl (funcall rpl)))
    ;; We already captured header arguments in the local `header' variable.
    ;; So we can just delete the structtempl line.
    (delete-region (line-beginning-position) (line-end-position))
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
		    (abbreviate-file-name (read-file-name "File: ")))
		  "\"")
		 t t rpl)))
    (unwind-protect
	(progn
	  (insert rpl)
	  (set-marker end-marker (point))
	  (forward-line 0)
	  (indent-to ind)
	  (forward-line)
	  ;; It might be that the content contains a question mark.
	  ;; Therefore we search for the position marker before we insert the contents.
	   (when (re-search-backward "\\?" start t)
	     (delete-char 1)
	     (set-marker pt-marker (point)))
	   (goto-char start)
	   (when (string-match "\n" rpl)
	     (indent-to ind))
	   (when (looking-at "[[:space:]]*#\\+\\(begin\\|BEGIN\\)_[[:alpha:]]+\\>")
	     (goto-char (match-end 0))
	     (insert header))
	   (goto-char end-marker)
	   ;; Search for the empty line where to insert the contents:
	   (when (re-search-backward "\n[[:space:]]*\n" start t)
	     (forward-line)
	     (delete-region (point) (line-end-position))
	     (insert content))
	   (when (marker-position pt-marker)
	     (goto-char pt-marker)))
      (set-marker end-marker nil)
      (set-marker pt-marker nil))))

(add-hook 'org-tab-after-check-for-cycling-hook #'org-structtempl-try-completion)


(defvar-local org-LaTeX-default-environment "align*"
  "Default environment for LaTeX snippets in org-mode.")

(add-to-list 'org-highlight-latex-and-related 'latex)

(defvar LaTeX-mode-hook) ;; defined in latex.el
(defun org-structure-template-latex ()
  "Return environment as string."
  (let* ((buf (or (buffer-file-name) "*org-latex-buffer*.tex"))
         (indent (current-indentation))
         (env org-LaTeX-default-environment)
         (ret
          (with-temp-buffer
            (let ((buffer-file-name buf)
                  LaTeX-mode-hook) ;; avoid long hook functions
              (insert "\\documentclass{article}\n\\usepackage{amsmath,amsfonts}\n\\begin{document}\n\\end{document}")
              (beginning-of-line)
              (latex-mode)
              (setq LaTeX-default-environment env)
              (LaTeX-environment nil)
              (setq env LaTeX-default-environment)
              (insert "?")
              (goto-char (point-min))
              (search-forward "\\begin{document}\n")
              (delete-region (point-min) (point))
              (search-forward "\\end{document}")
              (delete-region (match-beginning 0) (point-max))
              (goto-char (point-min)) (forward-line 1)
              (indent-rigidly (point) (point-max) indent)
              (buffer-string)))))
    (setq org-LaTeX-default-environment env)
    ret))

(provide 'org-structtempl)
;;; org-structtempl.el ends here
