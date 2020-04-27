# org-structtempl
Bring the good old structure templates back to org-mode.
There are problems with `org-tempo.el`.
For an instance, how would you substitue `<m` with the following string using `org-tempo.el`?
```lang-latex
\begin{align*}
?
\end{align*}
```
This is no problem for `org-structtempl.el`. Just add the following snippet to your `init.el`:
```lang-el
(with-eval-after-load 'org-structtempl
    (add-to-list 'org-structtempl-alist '("m" "\\begin{align*}\n?\n\\end{align*}")))
```
You can also customize `org-structtempl` instead.

# Where is `org+-structtempl.el`?
Note, that all features of `org+-structtempl.el` are now included in `org-structtempl.el`.
So, `org+-structtempl.el` became obsolete and has been deleted.

Please, replace `(require 'org+-structtempl)` by `(require 'org-structtempl)` in your init-file.
