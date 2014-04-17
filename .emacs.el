;;; Windows key is the new meta
(setq x-super-keysym 'meta)

;;; Swap C-u with C-x (for svorak, C-u == C-f)
(keyboard-translate ?\C-x ?\C-u)
(keyboard-translate ?\C-u ?\C-x)

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
; Lists (ctrl + d/h)
(global-set-key (kbd "C-d") 'backward-list)
(global-set-key (kbd "C-h") 'forward-list)

;;; Windmove commands
;
; Up/down
(global-set-key (kbd "C-x C-t") 'windmove-up)
(global-set-key (kbd "C-x C-n") 'windmove-down)
; Left/right
(global-set-key (kbd "C-x C-å") 'windmove-left)
(global-set-key (kbd "C-x C-ä") 'windmove-right)


;;; Bulgarian binds
;
;   Save/Find Buffer/File
(global-set-key (kbd "C-ф C-ъ") 'find-file)
(global-set-key (kbd "C-ф C-;") 'save-buffer)
(global-set-key (kbd "C-ф C-н") 'kill-region)
;  Copy/Paste
(global-set-key (kbd "C-,") 'kill-region)
(global-set-key (kbd "C-т") 'yank)
(global-set-key (kbd "M-,") 'kill-ring-save)
;   Start/End of line
(global-set-key (kbd "C-а") 'beginning-of-line)
(global-set-key (kbd "C-д") 'end-of-line)
;;; Prev/next-binds.
;
; Lines (ctrl + t/n)
(global-set-key (kbd "C-к") 'previous-line)
(global-set-key (kbd "C-л") 'next-line)
; Words (ctrl + å/ä)
(global-set-key (kbd "C-я") 'backward-word)
(global-set-key (kbd "C-в") 'forward-word)
; Chars (ctrl + ö/p)
(global-set-key (kbd "C-е") 'backward-char)
(global-set-key (kbd "C-р") 'forward-char)
; Paragraphs (win + t/n)
(global-set-key (kbd "M-к") 'backward-paragraph)
(global-set-key (kbd "M-л") 'forward-paragraph)
; Lists (ctrl + d/h)
(global-set-key (kbd "C-х") 'backward-list)
(global-set-key (kbd "C-й") 'forward-list)

;;; Windmove commands
;
; Up/down
(global-set-key (kbd "C-ф C-к") 'windmove-up)
(global-set-key (kbd "C-ф C-л") 'windmove-down)
; Left/right
(global-set-key (kbd "C-ф C-я") 'windmove-left)
(global-set-key (kbd "C-ф C-в") 'windmove-right)

;;; Rebind help-map
(global-set-key (kbd "C-x C-h") help-map)

;;; Set thresholds for when to split which direction
(setq split-height-threshold nil) ; no threshold
(setq split-width-threshold 80) ; at most 2 80 column windows at once

;;; Magit
(global-set-key (kbd "C-x C-g") 'magit-status)

;;; Line-numbers
(global-linum-mode t)

;;; Current time in mode-line
(display-time-mode 1)
(setq display-time-24hr-format 1) ;; 24hr clock
(setq display-time-day-and-date 1) ;; display date too

;;; Set up load-directory.
(let ((default-directory "~/.emacs-load/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; Package management with marmalade.
(require 'package)
(add-to-list 'package-archives
            '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
            '("geiser" . "http://download.savannah.gnu.org/releases/geiser/packages"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; annotate.el
(require 'annotate)

;;; Miscellaneous helper functions
(require 'helper-funcs)
(add-auto-indentation '(python-mode-hook
			emacs-lisp-mode-hook
			haskell-mode-hook))

;;; Quack (racket-stuff)
(require 'quack)
(add-auto-mode ".rkt" 'scheme-mode)

;;; Column marker to highlight columns
(require 'column-marker)
(add-hook 'scheme-mode-hook (lambda () (interactive) (column-marker-1 80)))
(global-set-key [?\C-c ?m] 'column-marker-1)
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
