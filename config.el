;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Arial" :size 24)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 32))

(setq doom-theme 'spacemacs-dark)

(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(after! org (setq org-hide-emphasis-markers t))

(add-hook! org-mode :append
           #'variable-pitch-mode)

;; hack because Doom doesn't seem to care about my frame size when restoring sessions ...
(setq initial-frame-alist '((top . 50) (left . 160) (width . 114) (height . 32)))

(fringe-mode '(80 . 80))                         ; Show vertical fringes

;; Yes, I really want to quit.
(setq confirm-kill-emacs nil)

(use-package! spaceline
  :config
  (spaceline-emacs-theme)
  (setq powerline-arrow-shape 'arrow))

(defvar my-use-boon t)
(defvar my-theme-shade "dark")

(use-package! boon
  :init
  (require 'boon-qwerty)
  (require 'boon-tutorial)
  (require 'boon-powerline)
  :config
  (if (eq my-use-boon t)
      (progn
        (boon-mode)
        (boon-powerline-theme)
        (setq boon-insert-cursor-color "orange")
        (if (equal my-theme-shade "dark")
            (progn
              (setq boon-default-cursor-color "white")
              (set-face-attribute 'boon-modeline-ins nil :background "orange" :foreground "black")
              )
          (setq boon-default-cursor-color "black")
          (set-face-attribute 'boon-modeline-ins nil :background "orange" :foreground "white")
          )
        (set-face-attribute 'boon-modeline-cmd nil :background "LightSkyBlue1" :foreground "black")
        (define-key boon-command-map "L" 'forward-sentence)
        (define-key boon-command-map "K" 'backward-sentence)
        (add-hook 'ibuffer-hook 'turn-off-boon-mode)
        (add-hook 'dired-hook-mode 'turn-off-boon-mode)
        (add-hook 'recentf-dialog-mode-hook 'turn-off-boon-mode)
        ))
  :bind
  ("C-c b" . turn-on-boon-mode)
  ("C-c e" . turn-off-boon-mode)
  ("C-;" . boon-set-command-state); used to quit insert mode
  )
