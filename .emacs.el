;;; Rebind help-map
(global-set-key (kbd "C-x C-h") help-map)

;;; Prev/next-binds.
;
; Lines (ctrl + t/n)
(global-set-key (kbd "C-t") 'previous-line)
(global-set-key (kbd "C-n") 'next-line)
; Words (ctrl + å/ä)
(global-set-key (kbd "C-å") 'backward-word)
(global-set-key (kbd "C-ä") 'forward-word)
; Chars (ctrl + ö/p)
(global-set-key (kbd "C-ö") 'backward-char)
(global-set-key (kbd "C-p") 'forward-char)
; Paragraphs (win + t/n)
(global-set-key (kbd "s-t") 'backward-paragraph)
(global-set-key (kbd "s-n") 'forward-paragraph)

;;; Annotate.el
(add-to-list 'load-path "~/annotate.el/")
(require 'annotate)

;;; Auto-indentation
(add-to-list 'load-path "~/auto-indent-hooks/")
(require 'auto-indent-hooks)
(add-auto-indentation)

;;; Swap C-u with C-x (for svorak, C-u == C-f)
(keyboard-translate ?\C-x ?\C-u)
(keyboard-translate ?\C-u ?\C-x)

;;; Binds for prog-modes.
(global-set-key [(control f1)] 'scheme-mode)

;;; Quack (racket-stuff)
(add-to-list 'load-path "~/quack/quack.el")
(require 'quack)

;;; Color-themes
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el/color-theme.el")
(require 'color-theme)
(eval-after-load "color-theme"
                 '(progn
                    (color-theme-initialize)
                    (color-theme-hober)))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-battery-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(quack-default-program "racket")
 '(quack-install t nil (quack))
 '(quack-pretty-lambda-p t)
 '(quack-run-scheme-always-prompts-p nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "#c0c0c0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Ubuntu Mono")))))
