;;$DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This is                                                                           ;;
;;          __                           __                                         ;;
;;     ____/ /___  ____  ____ ___   ____/ /                                         ;;
;;    / __  / __ \/ __ \/ __ `__ \ / __  /                                          ;;
;;  _/ /_/ / /_/ / /_/ / / / / / // /_/ /                                           ;;
;; (_)__,_/\____/\____/_/ /_/ /_(_)__,_/                                            ;;
;;                                                                                  ;;
;; My Emacs configuration.                                                          ;;
;; My Hotel California of creative writing.                                         ;;
;; Copyright (C) 2022 Jacob Moena                                                   ;;
;; Homepage: https://github.com/jacmoe/.doom.d                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Variables                                                                        ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-use-boon t)         ; use Boon-mode, or not
(defvar my-theme-shade "light") ; can be light or dark. Used to color the Boon-mode cursor
(defvar my-org-tracktable-daily-goal 1000) ; How many words do I want to write per day?
(defvar my-line-spacing 8) ; how much space between the lines?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Personal Information                                                             ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Jacob Moena"
      user-mail-address "jacmoe.dk@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; General settings                                                                 ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(save-place-mode 1)           ; Remember and restore the last cursor location of opened files
(setq confirm-kill-emacs nil) ; Yes, I really want to quit.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Visual settings                                                                  ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fancy splash screen
(setq fancy-splash-image (expand-file-name "splash/emacs.png" doom-private-dir))
;; Remove all but the first menu entry on the splash screen
(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 1))

;; Fonts - ordinary and variable pitch
(if (eq system-type 'windows-nt)
    (setq doom-font (font-spec :family "Lucida Console" :size 26) ; Windows font
          doom-variable-pitch-font (font-spec :family "ETBembo" :size 32))
  (setq doom-font (font-spec :family "Andale Mono" :size 26) ; Linux font
        doom-variable-pitch-font (font-spec :family "ETBembo" :size 32))
  )

;; Theme
;; (setq doom-theme 'doom-ayu-mirage)
(setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'doom-opera)
;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-nord-light)

;; Make the modified file name in the modeline orange instead of red
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

;; Setting initial size and position of frame
;; It is a necessary hack because Doom doesn't seem to
;; care about my frame size when restoring sessions ...
(if (eq system-type 'windows-nt)
    (setq initial-frame-alist '((top . 45) (left . 76) (width . 90) (height . 35)))
  (setq initial-frame-alist '((top . 45) (left . 76) (width . 90) (height . 30)))
  )

;; Misc settings
(setq display-line-numbers-type nil)        ; do not show line numbers
(display-time-mode 1)                       ; display time in modeline
(fringe-mode '(160 . 160))                  ; show vertical fringes
(blink-cursor-mode t)                       ; the cursor should blink
(setq-default line-spacing my-line-spacing) ; The amount of space between lines in pixels

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Global keybindings                                                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! "<f9>" #'doom-big-font-mode)   ; Toggle big font mode
(map!"C-<down>" #'enlarge-window)
(map!"C-<up>" #'shrink-window)
(map!"C-<left>" #'enlarge-window-horizontally)
(map!"C-<right>" #'shrink-window-horizontally)
(map! "C-`" #'diff-buffer-with-file) ; view what is modified

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Features                                                                         ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;; Org-tracktable                                                                   ;;
;; Org-appear
;; Org-ol-tree
;; Annotate
;; ISpell
;; Abbrev
;; Boon
;; CtrlF
;; Transparency
;; Move-text
;; Flymake-proselint
;; Mw-thesaurus
;; Emacs-powerthesaurus
;; Browse-kill-ring
;; Dimmer
;; Yasnippet
;; Miscellaneous

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-mode                                                                         ;;
;;                                                                                  ;;
;; https://orgmode.org/                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A GNU Emacs major mode for keeping notes, authoring documents, computational notebooks,
;; literate programming, maintaining to-do lists, planning projects,
;; and more — in a fast and effective plain text system.
(setq org-directory "~/org/")
(after! org (setq org-hide-emphasis-markers t))

(add-hook! org-mode :append
           #'visual-line-mode
           #'solaire-mode
           #'variable-pitch-mode)


(add-hook! org-mode (hl-line-mode -1))
(add-hook! org-mode (org-indent-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-tracktable                                                                   ;;
;;                                                                                  ;;
;; https://github.com/tty-tourist/org-tracktable                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an emacs/org-mode package for tracking your writing progress in an org-table.
(use-package! org-tracktable
  :config
  (setq org-tracktable-daily-goal my-org-tracktable-daily-goal))
(defalias 'tti 'org-tracktable-insert-table)
(defalias 'ttw 'org-tracktable-write)
(defalias 'tts 'org-tracktable-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-appear                                                                       ;;
;;                                                                                  ;;
;; https://github.com/awth13/org-appear                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make invisible parts of Org elements appear visible.
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-ol-tree                                                                      ;;
;;                                                                                  ;;
;; https://github.com/Townk/org-ol-tree                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This package offers an Org mode outline window that does not use indirect buffers.
(use-package! org-ol-tree
  :commands org-ol-tree)
(map! :map org-mode-map
      :after org
      "C-c t o" #'org-ol-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Annotate                                                                         ;;
;;                                                                                  ;;
;; https://github.com/bastibe/annotate.el                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add annotations to arbitrary files without changing the files themselves.
(use-package! annotate
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; ispell                                                                           ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive spelling
(use-package! ispell
  :config
  (setq ispell-dictionary "en")
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^# {{{" . "^# }}}"))
  :bind (("<f12>" . ispell-buffer)
         ("S-<f12>" . ispell-word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Abbrev                                                                           ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-text for Emacs
(setq save-abbrevs 'silently)     ; stop asking whether to save newly added abbrev when quitting emacs

(add-hook 'doom-first-buffer-hook ; one abbrev file for all modes
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Boon                                                                             ;;
;;                                                                                  ;;
;; https://github.com/jyp/boon                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        (add-hook 'doom-dashboard-mode 'turn-off-boon-mode)
        ))
  :bind
  ("<f6>" . turn-on-boon-mode)
  ("<f7>" . turn-off-boon-mode)
  ("C-;" . boon-set-command-state); used to quit insert mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; CtrlF                                                                            ;;
;;                                                                                  ;;
;; https://github.com/raxod502/ctrlf                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! ctrlf
  :config
  (ctrlf-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Transparency                                                                     ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Move-text                                                                        ;;
;;                                                                                  ;;
;; https://github.com/emacsfodder/move-text                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! move-text
  :init
  (move-text-default-bindings)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Flymake-proselint                                                                ;;
;;                                                                                  ;;
;; https://github.com/manuel-uberti/flymake-proselint                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! flymake-proselint
  :init
  (add-hook! #'text-mode-hook (lambda ()
                              (flymake-mode)
                              (flymake-proselint-setup)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Mw-thesaurus                                                                     ;;
;;                                                                                  ;;
;; https://github.com/agzam/mw-thesaurus.el                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! mw-thesaurus
  :init
  (if (boundp 'my-mw-api-key)
      (setq mw-thesaurus--api-key my-mw-api-key))
  :bind
  ("<f8>" . mw-thesaurus-lookup-dwim)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Emacs-powerthesaurus                                                             ;;
;;                                                                                  ;;
;; https://github.com/SavchenkoValeriy/emacs-powerthesaurus                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! powerthesaurus
  :bind
  ("<f5>" . powerthesaurus-lookup-synonyms-dwim)
  ("S-<f5>" . powerthesaurus-lookup-antonyms-dwim)
  )
(defalias 'pt-deft 'powerthesaurus-lookup-definitions-dwim)
(defalias 'pt-sent 'powerthesaurus-lookup-sentences-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Browse-kill-ring                                                                 ;;
;;                                                                                  ;;
;; https://github.com/browse-kill-ring/browse-kill-ring                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! browse-kill-ring
  :bind
  ("M-y" . 'browse-kill-ring)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dimmer                                                                           ;;
;;                                                                                  ;;
;; https://github.com/gonewest818/dimmer.el                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! dimmer
:init
(dimmer-mode t)
(setq dimmer-adjustment-mode :foreground)
(setq dimmer-fraction 0.30))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Yasnippet                                                                        ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! yasnippet
:bind
   ("C-c s n" . yas-new-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Miscellaneous                                                                    ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch to Danish keyboard layout
(defun my/kbdk ()
  (interactive)
  (call-process-shell-command "setxkbmap" nil nil nil "dk")
  )
(map! "C-c k" #'my/kbdk)

;; switch to American keyboard layout
(defun my/kbus ()
  (interactive)
  (call-process-shell-command "setxkbmap" nil nil nil "us")
  )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; better comment box                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jcs-comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))
