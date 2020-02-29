;;; org+-structtempl.el --- Supplement to org-structtempl.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tobias Zawada

;; Author: Tobias Zawada <naehring@smtp.1und1.de>
;; Keywords: wp, convenience

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

;; Add-ons to org-structtempl.el.

;;; Code:

;;; from lispTZA.el:
(defmacro save-point (&rest body)
  "Save point and do body like progn. Restore point afterwards."
  (declare (debug (body)))
  (let ((pt (make-symbol "pt")))
    `(let ((,pt (point)))
       (prog1
           (progn
             ,@body)
         (goto-char ,pt)))))

(defvar-local org-LaTeX-default-environment "align*"
  "Default environment for LaTeX snippets in org-mode.")

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

(setq org-highlight-latex-and-related '(latex))
(add-to-list 'org-structtempl-alist '("m" org-structure-template-latex org-structure-template-latex))

(defun tza-advice-org-complete-expand-structure-template (args)
  "Admit forms as replacements in `org-structure-template-alist'.
This is a :filter advice with ARGS."
  (let* ((start (nth 0 args))
	 (cell (nth 1 args)))
    (if (stringp (cdr cell))
	(let ((block-name (cdr cell)))
	  (list
	   start
	   (list (save-excursion
		   (goto-char start)
		   (looking-at "<\\([[:alnum:]]+\\)")
		   (match-string 1))
		 (format "#+begin_%s\n?\n#+end_%s" block-name block-name)
		 "")))
      (let* ((musep (bound-and-true-p org-mtags-prefer-muse-templates))
	     (rpl (nth (if musep 2 1) cell)))
	(unless (stringp rpl) ;; Otherwise pass args as they are.
	  (setq rpl (funcall rpl))
	  (setq cell (cl-copy-list cell)) ;; Don't modify `org-structure-template-alist'!
	  (setf (nth (if musep 2 1) cell) rpl))
	(list start cell)))))

;; (advice-add 'org-complete-expand-structure-template :filter-args #'tza-advice-org-complete-expand-structure-template)
(advice-add 'org-structtempl-expand :filter-args #'tza-advice-org-complete-expand-structure-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-structure-template-at-point ()
  "Determine structure template type at point and move point behind it.
Return nil and do not modify point if there is no structure template at point."
  (let (template
	point
	(e (line-end-position)))
    (save-excursion
      (forward-line -1)
      (when (search-forward-regexp (concat "^[[:space:]]*<\\(["
					   (apply 'concat (mapcar #'car org-structure-template-alist))
					   "]\\)\\_>")
				   e t)
	(setq point (point)
	      template (match-string-no-properties 1))))
    (when point
      (goto-char point))
    template))

(defun org+-whitespace-char-p (ch)
  "Return non-nil if CH is a whitespace character."
  (eq (char-syntax ch) ?\s))

(defun org+-rest-of-line-delete&return (&optional point ignore-space-bounds)
  "Delete rest of line starting at POINT and return it as string.
POINT defaults to `point'.
With non-nil IGNORE-SPACE-BOUNDS remove leading and trailing whitespace
from return string."
  (unless point
    (setq point (point)))
  (save-point
   (goto-char point)
   (let* ((end (line-end-position))
	  (str (buffer-substring-no-properties
		point
		end)))
     (delete-region point end)
     (if ignore-space-bounds
	 (substring
	  str
	  (cl-position-if-not #'org+-whitespace-char-p str)
	  (1+ (or (cl-position-if-not #'org+-whitespace-char-p str :from-end t)
		  (1- (length str)))))
       str))))
;; Test: (org+-rest-of-line-delete&return) Delete this stuff. 
;; Test: (org+-rest-of-line-delete&return nil t) Delete this stuff. 

(defun org-try-structure-completion-TZA (oldfun &rest args)
  "Take active region into consideration and add indentation for LaTeX-commands."
  (let (ret header
	(b-template (make-marker))
	(e-template (make-marker)))
    (unwind-protect
	(progn
	  (set-marker b-template (line-beginning-position))
	  (set-marker e-template (line-beginning-position 2))
	  (if (use-region-p)
	      ;; Save boundaries of region as markers. Do not kill region if this is not required!
	      (let* ((b (region-beginning))
		     (b-region (make-marker))
		     (e-region (make-marker)))
		(unwind-protect
		    (progn
		      (set-marker b-region b)
		      (set-marker e-region (region-end))
		      (save-excursion
			(goto-char b)
			(setq ret (org-structure-template-at-point)
			      b (point))
			(set-marker b-template (line-beginning-position))
			(set-marker e-template (line-beginning-position 2))
			(set-marker b-region (line-beginning-position 2)))
		      (when ret
			(goto-char b)
			(setq
			 header (org+-rest-of-line-delete&return nil t)
			 ret (apply oldfun args))
			(let ((pt (point)))
			  ;; if we find a newline within the template we put region there:
			  (goto-char b)
			  (when (re-search-forward "^[[:space:]]*$" b-region t)
			    (let ((str (buffer-substring b-region e-region)))
			      (delete-region b-region e-region)
			      (replace-match str t t)))
			  (goto-char pt)
			  (insert header))))
		  (set-marker b-region nil)
		  (set-marker e-region nil)))
	    (setq ret (apply oldfun args)))
	  ;; (when ret
	  ;;   (save-excursion
	  ;;     (goto-char b-template)
	  ;;     (looking-at "^[[:blank:]]*")
	  ;;     (let ((indent (match-string-no-properties 0)))
	  ;; 	(while (and (= (forward-line) 0)
	  ;; 		    (< (point) e-template))
	  ;; 	  (looking-at "^[[:blank:]]*")
	  ;; 	  (replace-match indent)))))
	  )
      (set-marker b-template nil)
      (set-marker e-template nil))
    ret))

;; (advice-add 'org-try-structure-completion :around #'org-try-structure-completion-TZA)
(advice-add 'org-structtempl-try-completion :around #'org-try-structure-completion-TZA)

(provide 'org+-structtempl)
;;; org+-structtempl.el ends here
