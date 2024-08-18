(setq   display-line-numbers-type       'relative
        org-directory                   "~/org/"
        orderless-component-separator   #'orderless-escapable-split-on-space
        orderless-matching-styles       '(orderless-flex)
)

(add-hook 'window-setup-hook #'toggle-frame-maximized)
(add-hook 'window-setup-hook #'treemacs)

(evil-define-operator sr/evil-delete-to-void-register (beg end type register)
  "Delete text from BEG to END with TYPE, placing it in the void register."
  :move-point nil
  :type exclusive
  (evil-delete beg end type ?_))

(defun sr/org-insert-code-block ()
  "Insert a code block with the specified LANG."
  (interactive)
  (let ((lang (completing-read "Language:" (mapcar #'cdr org-src-lang-modes))))
    (insert (format "#+begin_src %s\n\n#+end_src\n" lang))
    (previous-line 2)))  ;; Move cursor between the source block lines

(map! :map 'override "C-M-s-<f12>" #'execute-extended-command)

(map! :leader
      :desc "Consult buffer" "," #'consult-buffer)

(map! :n "r" #'sr/evil-delete-to-void-register)

(use-package! dired-subtree
  :after dired)

(use-package! orderless
  :after vertico
  :custom
  (completion-styles '(orderless)))

(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*"
               (display-buffer-reuse-window
                display-buffer-pop-up-window)
               (reusable-frames . t)))
