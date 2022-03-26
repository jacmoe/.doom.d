;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is
;;         __                           __
;;    ____/ /___  ____  ____ ___   ____/ /
;;   / __  / __ \/ __ \/ __ `__ \ / __  /
;; _/ /_/ / /_/ / /_/ / / / / / // /_/ /
;;(_)__,_/\____/\____/_/ /_/ /_(_)__,_/
;;
;; My Emacs configuration, optimized for creative writing
;; Copyright (C) 2017-2022 Jacob Moen
;; Author: Jacob Moen <jacmoe.dk@gmail.com>
;; Homepage: https://github.com/jacmoe/.doom.d
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-use-boon t)
(defvar my-theme-shade "dark")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)
;; Yes, I really want to quit.
(setq confirm-kill-emacs nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Visual settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fancy splash screen
(let ((alternatives '("doom-emacs-color2.svg"
                      "doom-emacs-color2.png"
                      "doom-emacs-color.png")))
  (setq fancy-splash-image
        (concat doom-private-dir "splash/"
                (nth (random (length alternatives)) alternatives))))

;; Remove all but the first menu entry on the splash screen
(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 1))

;; Fonts - ordinary and variable pitch
(setq doom-font (font-spec :family "Lucida Console" :size 26)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 32))

;; Theme
;; (setq doom-theme 'doom-opera)
(setq doom-theme 'doom-ayu-mirage)
;; (setq doom-theme 'doom-nord)

;; Setting initial size and position of frame
;; It is a necessary hack because Doom doesn't seem to
;; care about my frame size when restoring sessions ...
(setq initial-frame-alist '((top . 50) (left . 100) (width . 100) (height . 30)))

;; Misc settings
(setq display-line-numbers-type nil) ; do not show line numbers
(display-time-mode 1)                ; display time in modeline
(fringe-mode '(80 . 80))             ; show vertical fringes
(blink-cursor-mode t)                ; the cursor should blink

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global keybindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! "<f9>" #'doom-big-font-mode)          ; Toggle big font mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Features
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;; ISpell
;; Boon
;; CtrlF
;; Transparency
;; Move-text
;; Flymake-proselint
;; Mw-thesaurus
;; Emacs-powerthesaurus
;; Miscellaneous


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org-mode
;;
;; https://orgmode.org/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A GNU Emacs major mode for keeping notes, authoring documents, computational notebooks,
;; literate programming, maintaining to-do lists, planning projects,
;; and more — in a fast and effective plain text system.
(setq org-directory "~/org/")
(after! org (setq org-hide-emphasis-markers t))

(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)

(add-hook! org-mode :append
           #'solaire-mode)

(add-hook! org-mode (hl-line-mode -1))
(add-hook! org-mode (org-indent-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ISpell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive spelling
(use-package! ispell
  :config
  (setq ispell-dictionary "en")
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^# {{{" . "^# }}}"))
  :bind (("<f12>" . ispell-buffer)
         ("S-<f12>" . ispell-word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Boon
;;
;; https://github.com/jyp/boon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An Ergonomic Command Mode for Emacs
;; Run tutorial with M-x boon-tutorial
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
  ("<f6>" . turn-on-boon-mode)
  ("<f7>" . turn-off-boon-mode)
  ("C-;" . boon-set-command-state); used to quit insert mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CtrlF
;;
;; https://github.com/raxod502/ctrlf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! ctrlf
  :config
  (ctrlf-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Transparency
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggles background transparency
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 50) '(100 . 100)))))

(map! "C-c t t" #'toggle-transparency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Move-text
;;
;; https://github.com/emacsfodder/move-text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! move-text
  :init
  (move-text-default-bindings)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flymake-proselint
;;
;; https://github.com/manuel-uberti/flymake-proselint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! flymake-proselint
  :init
  (add-hook! #'text-mode-hook (lambda ()
                              (flymake-mode)
                              (flymake-proselint-setup)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mw-thesaurus
;;
;; https://github.com/agzam/mw-thesaurus.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! mw-thesaurus
  :init
  (if (boundp 'my-mw-api-key)
      (setq mw-thesaurus--api-key my-mw-api-key))
  :bind
  ("<f8>" . mw-thesaurus-lookup-dwim)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs-powerthesaurus
;;
;; https://github.com/SavchenkoValeriy/emacs-powerthesaurus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! powerthesaurus
  :bind
  ("<f5>" . powerthesaurus-lookup-synonyms-dwim)
  ("S-<f5>" . powerthesaurus-lookup-antonyms-dwim)
  )
(defalias 'pt-deft 'powerthesaurus-lookup-definitions-dwim)
(defalias 'pt-sent 'powerthesaurus-lookup-sentences-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch to Danish keyboard layout
(defun my/kbdk ()
  (interactive)
  (call-process-shell-command "setxkbmap" nil nil nil "dk")
  )

;; switch to American keyboard layout
(defun my/kbus ()
  (interactive)
  (call-process-shell-command "setxkbmap" nil nil nil "us")
  )

(map! "C-c k" #'my/kbdk)
(map! "C-c u" #'my/kbus)

;; kill current buffer, without confirmation
(defun delete-current-buffer ()
                                        ; deletes the current buffer
  (interactive)
  (kill-buffer (current-buffer)))

(map! "C-x k" #'delete-current-buffer)

;; align comments
(defun my-align-comments (beginning end)
  "Align comments within marked region."
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beginning end (concat "\\(\\s-*\\)"
                                        (regexp-quote comment-start)))))
