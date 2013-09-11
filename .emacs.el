;;; Windows key is the new meta
(setq x-super-keysym 'meta)

;;; Set up load-directory.
(let ((default-directory "~/.emacs-load/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; Package management with marmalade.
(require 'package)
(add-to-list 'package-archives
            '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

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
(global-set-key (kbd "M-t") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;;; annotate.el
(require 'annotate)

;;; Miscellaneous helper functions
(require 'helper-funcs)
(add-auto-indentation '(emacs-lisp-mode-hook python-mode-hook))

;;; Quack (racket-stuff)
(require 'quack)
(add-auto-mode ".rkt" 'scheme-mode) ; explained in helper-funcs.el

;;; Swap C-u with C-x (for svorak, C-u == C-f)
(keyboard-translate ?\C-x ?\C-u)
(keyboard-translate ?\C-u ?\C-x)


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
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(quack-default-program "racket")
 '(quack-install t nil (quack))
 '(quack-pretty-lambda-p t)
 '(quack-run-scheme-always-prompts-p nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#202020" :foreground "#c0c0c0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(quack-pltish-defn-face ((((class color) (background dark)) (:foreground "#5060ff")))))
