;; Org Roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/OrgFiles/zettelkasten"))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(provide 'config-orgmode)
