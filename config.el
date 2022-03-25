;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Arial" :size 24)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 32))

(setq doom-theme 'spacemacs-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; hack because Doom doesn't seem to care about my frame size when restoring sessions ...
(setq initial-frame-alist '((top . 50) (left . 160) (width . 114) (height . 32)))

;; this doesn't work
(fringe-mode '(80 . 80))                         ; Show vertical fringes

;; Yes, I really want to quit.
(setq confirm-kill-emacs nil)
