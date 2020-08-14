(setq user-full-name "Simon Christensen"
      user-mail-address "c3lphie@protonmail")

(setq doom-font (font-spec :family "monospace" :size 15))

(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type t)

(set-frame-parameter (selected-frame) 'alpha '(100 .85))

(setq org-startup-with-inline-images t)
(setq org-export-coding-system 'utf-8)

(setq org-directory "~/repositories/notes/")

(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
    '(("h" "lektier" entry (file+headline "~/repositories/notes/homework.org" "Todo")
       "** [ ] %^{Fag}: %^{Opgave} \n DEADLINE: %^{Deadline}T")
      ))
