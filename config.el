;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(let ((alternatives '("doom-emacs-color2.svg"
                      "doom-emacs-color2.png"
                      "doom-emacs-color.png")))
  (setq fancy-splash-image
        (concat doom-private-dir "splash/"
                (nth (random (length alternatives)) alternatives))))

(setq doom-font (font-spec :family "Arial" :size 24)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 28))

(setq doom-theme 'spacemacs-dark)

(setq display-line-numbers-type nil)
(display-time-mode 1)                           ;; display time in modeline

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

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

(defvar my-use-boon t)
(defvar my-theme-shade "dark")

(use-package! boon
  :init
  (require 'boon-qwerty)
  (require 'boon-tutorial)
  :config
  (if (eq my-use-boon t)
      (progn
        (boon-mode)
        (setq boon-insert-cursor-color "orange")
        (if (equal my-theme-shade "dark")
            (progn
              (setq boon-default-cursor-color "white")
              )
          (setq boon-default-cursor-color "black")
          )
        (define-key boon-command-map "L" 'forward-sentence)
        (define-key boon-command-map "K" 'backward-sentence)
        (add-hook 'ibuffer-hook 'turn-off-boon-mode)
        ))
  :bind
  ("C-c b" . turn-on-boon-mode)
  ("C-c e" . turn-off-boon-mode)
  ("C-;" . boon-set-command-state); used to quit insert mode
  )
