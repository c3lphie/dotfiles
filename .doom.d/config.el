(setq user-full-name "Simon Christensen"
      user-mail-address "c3lphie@protonmail")

(setq doom-font (font-spec :family "monospace" :size 15))

(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type t)

(set-frame-parameter (selected-frame) 'alpha '(100 . 100))

(setq org-startup-with-inline-images t)
(setq org-export-coding-system 'utf-8)
(setq org-hide-emphasis-markers t)

(setq org-directory "~/repositories/notes/")

(define-key global-map "\C-cc" 'org-capture)

(after! org
  (setq org-capture-templates
        '(("s" "Skole")
          ("se" "Basal elektronik" entry (file "~/repositories/notes/"))
          ("sl" "Problembaseret læring" entry (file "~/repositories/notes/"))
          ("sp" "Imperativ programmering")
          ("sm" "Lineær algebra")
          ("sh" "Lektier" entry (file+headline "~/repositories/notes/homework.org" "Todo")
           "** TODO %^{Fag}: %?\n DEADLINE: %^{Deadline}T")
          )))

(add-hook 'org-after-todo-state-change-hook 'dk/refile-todo 'append)
(defun dk/refile-todo()
(if (equal org-state "DONE")
	(dk/refile-to "~/repositories/notes/homework.org" "Done")
  (if (equal org-state "STRT")
	  (dk/refile-to "~/repositories/notes/homework.org" "In Progress")))
)

(defun dk/refile-to (file headline)
"Move current headline to specified location"
(let ((pos (save-excursion
			 (find-file file)
			 (org-find-exact-headline-in-buffer headline))))
  (org-refile nil nil (list headline file nil pos)))
(switch-to-buffer (current-buffer))
)
