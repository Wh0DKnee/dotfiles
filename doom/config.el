(setq   display-line-numbers-type       'relative
        org-directory                   "~/org/"
        orderless-component-separator   #'orderless-escapable-split-on-space
        orderless-matching-styles       '(orderless-flex))

(add-hook 'window-setup-hook #'toggle-frame-maximized)
(add-hook 'window-setup-hook #'treemacs)

(evil-define-operator sr/evil-delete-to-void-register (beg end type _register yank-handler)
  "Delete text from BEG to END with TYPE, placing it in the void register."
  (interactive "<R><x><y>")
  (evil-delete beg end type ?_ yank-handler))

(defun sr/evil-delete-to-end-of-line-to-void-register ()
  "Delete from the point to the end of the line into the void register."
  (interactive)
  (evil-delete (point) (line-end-position) 'inclusive ?_))

(defun sr/delete-char-to-void-register ()
  "Delete the character under the cursor and place it in the void register."
  (interactive)
  (let ((beg (point))
        (end (1+ (point)))) ;; end is one character after the point
    (evil-delete beg end 'exclusive ?_)))

(defun sr/org-insert-code-block ()
  "Insert a code block with the specified LANG."
  (interactive)
  (let ((lang (completing-read "Language:" (mapcar #'cdr org-src-lang-modes))))
    (insert (format "#+begin_src %s\n\n#+end_src\n" lang))
    (previous-line 2)))  ;; Move cursor between the source block lines

(defun eshell/async (&rest args)
  "Concatenate all ARGS into a single string and pass that string
   to async-shell-command."
  (async-shell-command (string-join args " ")))

(map! :map 'override "C-M-s-<f12>" #'execute-extended-command)

(map! :leader
      :desc "Consult buffer" "," #'consult-buffer)

(map! :n "r" #'sr/evil-delete-to-void-register)
(map! :n "R" #'sr/evil-delete-to-end-of-line-to-void-register)
(map! :n "x" #'sr/delete-char-to-void-register)

(use-package! dired-subtree
  :after dired)

(use-package! orderless
  :after vertico
  :custom
  (completion-styles '(orderless)))

(after! evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t))

(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*"
               (display-buffer-reuse-window
                display-buffer-pop-up-window)
               (reusable-frames . t)))
