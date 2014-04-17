(defun add-auto-mode (extension prog-mode)
  "Adds a binding for a prog-mode to a given file extension"
  (setq auto-mode-alist
	(append
	 (list
	  (cons (concat "\\" extension "\\'") prog-mode))
	 auto-mode-alist)))

; Set up the binding to use as RET rebind.
(global-set-key (kbd "C-j") 'newline-and-indent)

; Define the config we want to use.
(defun auto-indent-coding-config ()
  (local-set-key (kbd "RET") (key-binding (kbd "C-j")))
  (local-set-key (kbd "<S-return>") 'newline)
  )

(defun insert-auto-indent-hook (language-mode-hook)
  "Adds auto-indentation to the selected prog-mode"
   (add-hook language-mode-hook 'auto-indent-coding-config))

; Add auto-indentation to the specified languages.
; (mapc FUNCTION to apply to LIST)
(defun add-auto-indentation (prog-mode-hooks)
  (mapc 'insert-auto-indent-hook prog-mode-hooks))

(provide 'helper-funcs)
