;; -*- lexical-binding: t; -*-
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
;; Copyright (C) 2022-2023 Jacob Moena                                              ;;
;; Homepage: https://github.com/jacmoe/.doom.d                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Variables                                                                       ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-keyboard-variant "qwerty")                 ; colemak or qwerty

(defvar my-light-theme 'sandcastle)
(defvar my-dark-theme 'doom-xcode)

(defvar my-main-theme my-dark-theme)
(defvar my-theme-shade "dark")                       ; can be light or dark.

(defvar my-monospace-font "Overpass Mono")      ; Font to use for code
(defvar my-variablespace-font "Overpass")             ; Font to use for writing ;Carlito;Alegreya
(defvar my-monospace-font-size 26)
(defvar my-variablespace-font-size 36)

(defvar my-org-tracktable-daily-goal 500)            ; How many words do I want to write per day?
(defvar my-day-end 5)                                ; when does my day end?

(defvar my-line-spacing 12)                          ; how much space between the lines?

(defvar my-org-pomodoro-play-sounds nil) ; don't play sounds
(defvar my-org-pomodoro-length 20)       ; 20 minutes are great for word-sprints
(defvar my-org-pomodoro-short-break-length 5)


;; Where do I store everything to be shared between machines?
(defvar my-storage-directory "~/Dropbox/skriv/")
(defvar my-personal-dictionary (concat my-storage-directory "aspell-en")) ; store personal dictionary here
(setq bookmark-default-file (concat my-storage-directory "bookmarks")) ; Where to save the bookmarks file
(setq annotate-file (concat my-storage-directory "annotations"))
(setq bib-file (concat my-storage-directory "jacmoe.bib"))
(defvar org-heatmap-db-location (concat my-storage-directory "org-heatmap.db"))

;; Org Directories
(setq +org-roam-auto-backlinks-buffer t
      org-directory (concat my-storage-directory "org/")
      org-roam-directory org-directory
      org-roam-dailies-directory (concat org-directory "roam-journal/")
      org-archive-location (concat org-directory ".archive/%s::")
      org-agenda-files (list org-directory
                             "~/enestaaende/enestaaende.org"
                             "~/Dropbox/skriv/habits/habits.org"))
                             
(defvar my-puppeteer-config-file "/home/moena/puppeteerConfigFile.json")

;; Harper configuration
(setq-default eglot-workspace-configuration
              '(:harper-ls (:userDictPath "~/Dropbox/skriv/harper-ls/dictionary.txt"
                            :fileDictPath "~/Dropbox/skriv/harper-ls/file_dictionaries/"
                            :ignoredLintsPath "~/Dropbox/skriv/harper-ls/ignored_lints/")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Personal Information                                                            ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Jacob Moena"
      user-mail-address "jacmoe.dk@gmail.com"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Secret Information                                                              ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load secret settings from .secret.el which needs to be created.
;; Can be API keys, login information, etc.
;; See secret.el.example
(setq secret-file (expand-file-name ".secret.el" doom-user-dir))
(when (file-exists-p secret-file)
  (load secret-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; General settings                                                                ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(save-place-mode 1)                                    ; Remember and restore the last cursor location of opened files
(setq confirm-kill-emacs nil)                          ; Yes, I really want to quit.
(setq inhibit-compacting-font-caches t)                ; for performance reasons
(setq bookmark-save-flag 1)                            ; Save bookmarks each time it changes, not only on exit
(setq enable-local-eval t)                             ; Define safe local variables
(unless (eq system-type 'windows-nt)                   ; Mouse-avoidance makes the frame "jump" on Windows...
  (mouse-avoidance-mode 'banish))                      ; Shove the mouse pointer out of  the way
(setq undo-fu-session-linear t)                        ; tell Undo-fu to only store linear history, not the full history
(map! "C-;" nil)                                       ; Don't steal my C-; !
(setq flycheck-emacs-lisp-load-path 'inherit)          ; Flycheck fix

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Visual settings                                                                 ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convince Emacs to trust themes so that we can install them
(setq custom-safe-themes t)
;; Fancy splash screen
(setq fancy-splash-image (expand-file-name "splash/emacs.png" doom-user-dir))

(setq +doom-dashboard-menu-sections
  '(
    ("Reload last session"
     :icon (nerd-icons-octicon "nf-oct-history" :face 'doom-dashboard-menu-title)
     :when (cond ((modulep! :ui workspaces)
                  (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                 ((require 'desktop nil t)
                  (file-exists-p (desktop-full-file-name))))
     :action doom/quickload-session)
    ("Jump to bookmark"
    :icon (nerd-icons-octicon "nf-oct-bookmark_fill" :face 'doom-dashboard-menu-title)
    :action bookmark-jump)
    ("Recently opened files"
    :icon (nerd-icons-octicon "nf-oct-file_directory_fill" :face 'doom-dashboard-menu-title)
    :action recentf-open-files)
    ("Open directory (project)"
     :icon (nerd-icons-octicon "nf-oct-briefcase" :face 'doom-dashboard-menu-title)
     :action projectile-switch-project)
    ("View open buffers"
    :icon (nerd-icons-octicon "nf-oct-stack" :face 'doom-dashboard-menu-title)
    :action ibuffer)
    ("Org-roam"
    :icon (nerd-icons-octicon "nf-oct-book" :face 'doom-dashboard-menu-title)
    :action org-roam-node-find)
    ("Open private configuration"
    :icon (nerd-icons-octicon "nf-oct-tools" :face 'doom-dashboard-menu-title)
    :when (file-directory-p doom-private-dir)
    :action doom/open-private-config)))

;; Set the title
(setq frame-title-format '("%b – Hotel California of Creative Writing"))
;; Add to the dashboard
(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Hotel California of Creative Writing")))

;; Fonts - ordinary and variable pitch
(setq doom-font (font-spec :family my-monospace-font :size my-monospace-font-size :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family my-variablespace-font :size my-variablespace-font-size)
      doom-big-font (font-spec :family my-monospace-font :size (+ my-monospace-font-size 2) :weight 'semi-light))

;; Theme
(setq doom-theme my-main-theme)

;; Make the modified file name in the modeline orange instead of red
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

;; Make the Doom modeline occupy a more comfortable amount of space
(setq doom-modeline-height 65)

(custom-set-faces
  '(mode-line ((t (:family "Noto Sans" :height 0.9))))
  '(mode-line-active ((t (:family "Noto Sans" :height 0.9)))) ; For 29+
  '(mode-line-inactive ((t (:family "Noto Sans" :height 0.9)))))

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes check input-method buffer-encoding major-mode process vcs "           "))) ; <-- added padding here

;; Remove annoying system load number after time
(setq display-time-default-load-average nil)
;; Because Emacs do not render italics at all when markup is hidden force it to render it differently
(custom-set-faces!
  '(italic :slant oblique :foreground "teal"))

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

;; Misc settings
(setq display-line-numbers-type nil)                        ; Do not show line numbers
(display-time-mode 1)                                       ; Display time in modeline
(setq display-time-24hr-format 1)                           ; Clock is using 24 hour format
(fringe-mode '(80 . 80))                                    ; Show vertical fringes
(blink-cursor-mode t)                                       ; The cursor should blink
(setq-default line-spacing my-line-spacing)                 ; The amount of space between lines in pixels
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode) ; Do not highlight current line
(setq global-page-break-lines-mode t)                       ; Pretty page breaks everywhere
(setq confirm-kill-processes nil)                           ; Don't ask to kill running processes when exiting Emacs.
(add-hook 'text-mode-hook (lambda () (setq-local line-spacing 0.1))) ; Setting a more comfortable line spacing for prose


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Global keybindings                                                              ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :desc "Show TrackTable status" "C-c h w" #'org-tracktable-status)
(map! :desc "Write to TrackTable" "C-c h W" #'org-tracktable-write)
(map! :desc "Grab link from browser" "C-c h g" #'grab-x-link)
(map! :desc "Go to last edit" "C-c h e" #'last-edit)
(map! :desc "Center cursor mode" "C-c h s" #'prot/scroll-center-cursor-mode)
(map! :desc "Diff buffer with file" "C-c h d" #'diff-buffer-with-file)
(map! :desc "Look up word in dictionary" "C-c h l" #'dictionary-lookup-definition)     ; Look up word in the dictionary

(map! :desc "Look up word in dictionary" "<f8>" #'dictionary-lookup-definition)     ; Look up word in the dictionary
(map! :desc "Toggle mode-line" "C-c t m" #'hide-mode-line-mode)           ; hide the mode-line
(map! :desc "Toggle light/dark theme" "C-c t d" #'switch-theme)                  ; switch theme light/dark

(map! :desc "Capture to this buffer" "C-c n q" #'tb/capture-to-this-buffer)     ; quick capture to this buffer

(map! :desc "Enlarge window" "C-<up>" #'enlarge-window)
(map! :desc "Shrink window" "C-<down>" #'shrink-window)
(map! :desc "Enlarge window horizontally" "C-<left>" #'enlarge-window-horizontally)
(map! :desc "Shrink window horizontally" "C-<right>" #'shrink-window-horizontally)
(map! "M-1" (lambda() (interactive) (org-shifttab 1)))
(map! "M-2" (lambda() (interactive) (org-shifttab 2)))
(map! "M-3" (lambda() (interactive) (org-shifttab 3)))
(map! "M-4" (lambda() (interactive) (org-shifttab 4)))
(map! "M-5" (lambda() (interactive) (org-show-all '(headings drawers blocks))))

 (if (equal my-keyboard-variant "colemak")
     (progn
       ;; using colemak
       (map! "C-o" #'boon-set-command-state); used to quit insert mode
       (map! "C-ø" #'open-line); remapping open-line, Danish version
       (map! "C-;" #'open-line)); remapping open-line

   (progn
     ;; using qwerty
     (map! "C-;" #'boon-set-command-state); used to quit insert mode
     (map! "C-æ" #'boon-set-command-state))); used to quit insert mode - Danish version

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings defined elsewhere:                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ("C-c h o" . org-side-tree)
;; ("C-c h O" . org-side-tree-toggle)
;; ("<F9>" . distraction-free)
;; ("C-c h z" . distraction-free)
;; ("<f6>" . eglot-code-actions) ; Deal with Harper errors
;; ("<f7>" . +default/diagnostics) ; list Harper errors
;; ("<f5>" . powerthesaurus-lookup-synonyms-dwim)
;; ("S-<f5>" . powerthesaurus-lookup-antonyms-dwim)
;; (map! "C-c k" #'my/kbdk)
;; (map! "C-c u" #'my/kbus)
;; ("C-c s n" . yas-new-snippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Aliases                                                                         ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'ts 'transpose-sentences)
(defalias 'tp 'transpose-paragraphs)
(defalias 'note 'tb/capture-to-this-buffer)
(defalias 'slip 'jethro/org-capture-slipbox)
(defalias 'tt-insert 'org-tracktable-insert-table)
(defalias 'tt-update 'org-tracktable-write)
(defalias 'tt-status 'org-tracktable-status)
(defalias 'fix-quotes 'smart-quotes-smarten)
(defalias 'mwebster 'mw-thesaurus-lookup-dwim)
(defalias 'pt-defs 'powerthesaurus-lookup-definitions-dwim)
(defalias 'pt-sent 'powerthesaurus-lookup-sentences-dwim)
(defalias 'dwp 'define-word-at-point)
(defalias 'dw 'define-word)
(defalias 'om 'org-mode)
(defalias 'tm 'text-mode)
(defalias 'kcast 'keycast-tab-bar-mode)
(defalias 'gcast 'gif-screencast)
(defalias 'free 'freeze-it-mode)
(defalias 'fro 'freeze-it-show)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Features                                                                        ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;; Org-Roam
;; Org-Roam-UI
;; Org-tracktable
;; Org-appear
;; Org-modern
;; Org-pomodoro
;; Org-habit-stats
;; Citar
;; Ox-Hugo
;; Org-heatmap
;; Annotate
;; ISpell
;; Magit
;; Abbrev
;; Typopunct
;; Colorful-mode
;; Smart-quotes
;; Boon
;; CtrlF
;; Org-side-tree
;; Hl-todo
;; Mw-thesaurus
;; Emacs-powerthesaurus
;; Browse-kill-ring
;; Dimmer
;; Harper
;; Yasnippet
;; Nov.el
;; Emacs-everywhere
;; Atomic-chrome
;; Keycast
;; Gif-screencast
;; Olivetti-mode
;; Freeze-it
;; Monkeytype
;; Miscellaneous
;; Better comment box
;; Last edit
;; Center scroll minor mode
;; Theme functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Org-mode                                                                        ;;
;;                                                                                  ;;
;; https://orgmode.org/                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A GNU Emacs major mode for keeping notes, authoring documents, computational notebooks,
;; literate programming, maintaining to-do lists, planning projects,
;; and more — in a fast and effective plain text system.

(add-hook! org-mode :append
           #'visual-line-mode
           #'typopunct-mode
           #'variable-pitch-mode)

;; Org-habit
(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 35
        org-habit-show-habits-only-for-today nil
        org-habit-graph-column 1
        org-habit-show-habits t))

;; use :ignore: tags to ignore the heading, but keep the content
(use-package! ox-extra
  :after org
  :config
  (ox-extras-activate '(ignore-headlines)))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(add-hook! org-mode (org-indent-mode -1))

(after! org
  (setq mermaid-flags (concat "-w 1810 --puppeteerConfigFile " my-puppeteer-config-file))

  (setq org-enforce-todo-dependencies t
        org-hierarchical-todo-statistics nil ; I want org-mode to cascade done statistics up through the tree
        org-todo-keyword-faces
        '(("todo" . "goldenrod1") ("fixme" . "goldenrod1") ("idea" . "goldenrod1")
          ("draft" . "coral2") ("revise" . "PaleGreen4")
          "|" ("done" . "DarkOrchid3"))
        org-todo-keywords
        '((sequence "draft(t)" "revise(r)" "|" "done(d)")
          (sequence "fixme" "|" "fixed")
          (sequence "todo" "|" "done"))
        org-tag-faces
        '(
          ("noexport" . (:foreground "#606060" :weight normal))
          ("nowc" . (:foreground "#606060" :weight normal))
          ("ignore" . (:foreground "#606060" :weight normal)))

        org-return-follows-link t ; hitting RETURN follows the link

        ;; We want to log the time when the TODO is closed
        org-log-done "time" org-log-done-with-time 't
        ;; If idle for more than 10 minutes, resolve the things
        ;; by asking what to do with the clock time
        org-clock-idle-time 10

        org-html-htmlize-output-type nil ; do not use inline css for HTML export

        org-latex-toc-command "\\tableofcontents \\clearpage" ; Force page break after TOC for PDF exports
        org-latex-image-default-width ""
        org-latex-image-default-scale "0.4"
        org-latex-images-centered nil
        org-latex-toc-command "\\clearpage \\tableofcontents \\clearpage"
        org-fontify-done-headline nil ; don't color the headline grey when done

       org-capture-templates
        '(("s" "Slipbox" entry  (file "inbox.org")
           "* %?\n%t\n%i\n%a"))
  ) ; setq
) ; after org

(after! org
(defun my/link-export (destination description backend)
  (let* ((link (concat "https://www.youtube.com/embed/" destination))
         (description (or description link)))
    (cond
     ((eq backend 'html)
      (format "<div><iframe width=\"560\" height=\"315\" src=\"%s\" frameborder=\"0\" allowfullscreen></iframe></div>" link description)))))
(org-link-set-parameters "yt" :follow 'my/link-export :export 'my/link-export)
)

(defun org-habit-streak-count ()
  (goto-char (point-min))
  (while (not (eobp))
    ;;on habit line?
    (when (get-text-property (point) 'org-habit-p)
      (let ((streak 0)
            streak-broken)
        (move-to-column org-habit-graph-column)
        ;;until end of line
        (while (not (eolp))
          (if (= (char-after (point)) org-habit-completed-glyph)
              (if streak-broken
                  (setq streak 1
                        streak-broken nil)
                (setq streak (+ streak 1)))
            (setq streak-broken t))
          (forward-char 1))
        (end-of-line)
        (insert (number-to-string streak))))
    (forward-line 1)))

(after! org
  (add-hook 'org-agenda-finalize-hook 'org-habit-streak-count)
  (setq org-agenda-custom-commands
        '(("h" "Daily habits"
           ((agenda ""))
           ((org-agenda-show-log t)
            (org-agenda-ndays 30)
            (org-agenda-log-mode-items '(state))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":daily:")))))))

(after! ox-latex
 (add-to-list 'org-latex-classes
              '("org-plain-latex"
                "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(defun tb/capture-to-this-buffer ()
  "Capture note to this buffer"
  (interactive)
  (cond  ((not  (eq major-mode 'org-mode))
          (message "Can't capture to non org-mode buffer"))
         (t
          (let* ((this-file buffer-file-name)
                 (org-capture-templates
                  `(("n" "Note" entry (file+headline ,this-file "Captured")
                     "** %?\n"))))
            (org-capture nil "n")))))

(defun jethro/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Org-Roam                                                                        ;;
;;                                                                                  ;;
;; https://github.com/org-roam/org-roam                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! org-roam
  (setq org-roam-capture-templates
        `(("n" "note" plain
           ,(format "#+title: ${title}\n%%[%s/template/note.org]" org-roam-directory)
           :target (file "note/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t))
        ;; Use human readable dates for dailies titles
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%B %d, %Y>\n\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Org-Roam-UI                                                                     ;;
;;                                                                                  ;;
;; https://github.com/org-roam/org-roam-ui                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Org-tracktable                                                                  ;;
;;                                                                                  ;;
;; https://github.com/tty-tourist/org-tracktable                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an emacs/org-mode package for tracking your writing progress in an org-table.
(use-package! org-tracktable
  :defer t
  :config
  (setq org-tracktable-daily-goal my-org-tracktable-daily-goal
        org-tracktable-day-delay my-day-end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Org-appear                                                                      ;;
;;                                                                                  ;;
;; https://github.com/awth13/org-appear                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make invisible parts of Org elements appear visible.
(use-package! org-appear
  :defer t
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
;;; Org-modern                                                                      ;;
;;                                                                                  ;;
;; https://github.com/minad/org-modern                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern style for your Org buffers
(use-package! org-modern
  :defer t
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Org-pomodoro                                                                    ;;
;;                                                                                  ;;
;; https://github.com/marcinkoziej/org-pomodoro                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-pomodoro
  :defer t
  :config
  (setq
   org-pomodoro-play-sounds my-org-pomodoro-play-sounds ; don't play sounds
   org-pomodoro-length my-org-pomodoro-length       ; 20 minutes are great for word-sprints
   org-pomodoro-short-break-length my-org-pomodoro-short-break-length
   ;; use libnotify
   alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Org-habit-stats                                                                 ;;
;;                                                                                  ;;
;; https://github.com/ml729/org-habit-stats/                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package! org-habit-stats
;; :after org
;; :init
;; (add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Citar                                                                           ;;
;;                                                                                  ;;
;; https://github.com/bdarcus/citar                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bibliography management
(after! citar
  (setq! citar-bibliography (list bib-file))
  (setq org-cite-global-bibliography citar-bibliography)
  (setq ;;citar-library-paths '("/path/to/library/files/")
   citar-notes-paths (list org-roam-directory)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Ox-Hugo                                                                         ;;
;;                                                                                  ;;
;; https://ox-hugo.scripter.co/                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org exporter backend that exports Org to Hugo-compatible Markdown
;; (after! ox-hugo
;;   (plist-put org-hugo-citations-plist :bibliography-section-heading "Bibliography"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Org-heatmap                                                                     ;;
;;                                                                                  ;;
;; https://github.com/Elilif/org-heatmap                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-heatmap
  :defer t
  :after org
  :config
  (org-heatmap-mode)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Annotate                                                                        ;;
;;                                                                                  ;;
;; https://github.com/bastibe/annotate.el                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add annotations to arbitrary files without changing the files themselves.
(use-package! annotate
  :defer t
  :config
  (setq annotate-database-confirm-deletion t))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Ispell                                                                          ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive spelling
(use-package! ispell
  :defer t
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en")
  (setq ispell-personal-dictionary my-personal-dictionary)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\|HEADER\\|SPELLING\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^# {{{" . "^# }}}"))
  :bind (("<f12>" . ispell-buffer)
         ("S-<f12>" . ispell-word)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Magit                                                                           ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! magit
  (setq magit-diff-refine-hunk 'all))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Abbrev                                                                          ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-text for Emacs
(setq save-abbrevs 'silently)   ; stop asking whether to save newly added abbrev when quitting emacs
(setq abbrev-file-name (expand-file-name "abbrev_defs.el" doom-private-dir)) ; where to load abbrevs from
(setq-default abbrev-mode t)    ; Abbrev mode always on


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Typopunct                                                                       ;;
;;                                                                                  ;;
;; https://github.com/emacsmirror/typopunct                                         ;;
;; https://www.emacswiki.org/emacs/TypographicalPunctuationMarks                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic typographical punctuation marks
(use-package! typopunct
  :config
  (typopunct-change-language 'english t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Colorful-mode                                                                   ;;
;;                                                                                  ;;
;; https://github.com/DevelopmentCool2449/colorful-mode                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preview any color in your buffer in real time.
(use-package! colorful-mode
  :custom
  ;; (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'org-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Smart-quotes                                                                    ;;
;;                                                                                  ;;
;; https://github.com/gareth-rees/smart-quotes                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only used for the "smart-quotes-smarten" function
(use-package! smart-quotes
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Boon                                                                            ;;
;;                                                                                  ;;
;; https://github.com/jyp/boon                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An Ergonomic Command Mode for Emacs
;; Run tutorial with M-x boon-tutorial
 (use-package! boon
   :defer t
   :init
   (require 'boon)
   :config
   (boon-mode)

   (if (equal my-keyboard-variant "colemak")
       ;; we are using Colemak
       (progn
         (define-key boon-moves-map "ø" 'move-end-of-line)
         (require 'boon-colemak-hnei)
         (define-key boon-moves-map "l" 'move-beginning-of-line)
         (define-key boon-moves-map ";" 'move-end-of-line))

     ;; we are using Qwerty
     (progn
       (define-key boon-moves-map "æ" 'boon-smarter-forward)
       (require 'boon-qwerty-hjkl)
       (define-key boon-moves-map "u" 'move-beginning-of-line)
       (define-key boon-moves-map "p" 'move-end-of-line)))

   ;; turn off Boon in the following modes
   (add-hook 'ibuffer-hook 'turn-off-boon-mode)
   (add-hook 'doom-dashboard-mode 'turn-off-boon-mode)
   (add-hook 'org-capture-mode-hook 'turn-off-boon-mode)
   (add-hook 'speed-type-mode-hook 'turn-off-boon-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; CtrlF                                                                           ;;
;;                                                                                  ;;
;; https://github.com/raxod502/ctrlf                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! ctrlf
  :config
  (ctrlf-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Org-side-tree                                                                   ;;
;;                                                                                  ;;
;; https://github.com/localauthor/org-side-tree                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-side-tree
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local outline-regexp ";;;\\(;* [^   \t\n]\\)")))
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  :bind
  ("C-c h o" . org-side-tree)
  ("C-c h O" . org-side-tree-toggle))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Hl-todo                                                                         ;;
;;                                                                                  ;;
;; https://github.com/tarsius/hl-todo                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! hl-todo
  :config
  (add-hook 'org-mode-hook 'hl-todo-mode)
(setq hl-todo-keyword-faces
      '(("ADD"   . "Palegreen4")
        ("FIX"  . "goldenrod2")
        ("IDEA"   . "Palegreen4")
        ("NOTE"  .  "DarkOrchid3")))
(keymap-set hl-todo-mode-map "C-c h p" #'hl-todo-previous)
(keymap-set hl-todo-mode-map "C-c h n" #'hl-todo-next))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Flycheck-hl-todo                                                                ;;
;;                                                                                  ;;
;; https://github.com/tarsius/flycheck-hl-todo                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! flycheck-hl-todo
  :config
(flycheck-hl-todo-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Mw-thesaurus                                                                    ;;
;;                                                                                  ;;
;; https://github.com/agzam/mw-thesaurus.el                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! mw-thesaurus
  :defer t
  :init
  (if (boundp 'my-mw-api-key)
      (setq mw-thesaurus--api-key my-mw-api-key)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Emacs-powerthesaurus                                                            ;;
;;                                                                                  ;;
;; https://github.com/SavchenkoValeriy/emacs-powerthesaurus                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! powerthesaurus
  :defer t
  :bind
  ("<f5>" . powerthesaurus-lookup-synonyms-dwim)
  ("S-<f5>" . powerthesaurus-lookup-antonyms-dwim))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Browse-kill-ring                                                                ;;
;;                                                                                  ;;
;; https://github.com/browse-kill-ring/browse-kill-ring                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! browse-kill-ring
  :defer t
  :bind
  ("M-y" . 'browse-kill-ring))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Dimmer                                                                          ;;
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
;;; Harper                                                                          ;;
;;                                                                                  ;;
;; https://github.com/automattic/harper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! eglot
  :hook
  (org-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(org-mode . ("/home/moena/bin/harper-ls" "--stdio")))
   :bind
   ("<f6>" . eglot-code-actions)
   ("<f7>" . +default/diagnostics))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Yasnippet                                                                       ;;
;;                                                                                  ;;
;; https://github.com/joaotavora/yasnippet                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! yasnippet
  :defer t
  :bind
  ("C-c s n" . yas-new-snippet))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Nov.el                                                                          ;;
;;                                                                                  ;;
;; https://depp.brause.cc/nov.el/                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! nov
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Emacs-everywhere                                                                ;;
;;                                                                                  ;;
;; https://github.com/tecosaur/emacs-everywhere                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! emacs-everywhere
  ;; Easier to match with a bspwm rule:
  ;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
  (setq emacs-everywhere-frame-name-format "emacs-anywhere")

  ;; The modeline is not useful to me in the popup window. It looks much nicer
  ;; to hide it.
  (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

  ;; Semi-center it over the target window, rather than at the cursor position
  ;; (which could be anywhere).
  (defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
    :override #'emacs-everywhere-set-frame-position
    (cl-destructuring-bind (x y width height)
        (emacs-everywhere-window-geometry window-info)
      (set-frame-position frame
                          (+ x (/ width 2) (- (/ width 2)))
                          (+ y (/ height 2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Atomic-chrome                                                                   ;;
;;                                                                                  ;;
;; https://github.com/alpha22jp/atomic-chrome                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! atomic-chrome
 :defer t
 :init
 (add-transient-hook! 'focus-out-hook (atomic-chrome-start-server)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Keycast                                                                         ;;
;;                                                                                  ;;
;; https://github.com/tarsius/keycast                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Gif-screencast                                                                  ;;
;;                                                                                  ;;
;; https://gitlab.com/ambrevar/emacs-gif-screencast                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :map gif-screencast-mode-map
        :g "<f8>" #'gif-screencast-toggle-pause
        :g "<f9>" #'gif-screencast-stop)
  (setq gif-screencast-program "maim"
        gif-screencast-args `("--quality" "3" "-i" ,(string-trim-right
                                                     (shell-command-to-string
                                                      "xdotool getactivewindow")))
        gif-screencast-optimize-args '("--batch" "--optimize=3" "--colors=256"))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Olivetti-mode                                                                   ;;
;;                                                                                  ;;
;; https://github.com/rnkn/olivetti                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Distraction-free screen
(use-package! olivetti
  :init
  (setq olivetti-body-width .54)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (text-scale-increase 1.6)
          (olivetti-mode t))
      (progn
        (olivetti-mode 0)
        (text-scale-decrease 1.6))))
  :bind
  (("<f9>" . distraction-free))
  (("C-c h z" . distraction-free)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Freeze-it
;;                                                                                  ;;
;; https://github.com/rnkn/freeze-it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! freeze-it
  :config
        (setq freeze-it-delay 1) ; delay in seconds
        (setq freeze-it-go-back 'line) ;one of word, line, visible-line, and paragragh
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Monkeytype
;;                                                                                  ;;
;; https://github.com/jpablobr/emacs-monkeytype
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! monkeytype)

(after! monkeytype
(defun functions/monkeytype-load-and-start (arg &optional file-name num-words)
    (interactive "p")
    (when (/= arg 1)
        (setf file-name (read-file-name "Insert name of file with words: "))
        (setf num-words (read-number "Insert number of words you require: " 50)))

    (let ((res '())
        (final-buffer "*Monkeytype-words*")
        (true-num-words (or num-words 50))
        (num-buffer-words nil)
        (indices nil))

        (with-temp-buffer
        (insert-file-contents
        (or file-name
        (expand-file-name
            "wikipedia2k.txt"
            "~/.monkeytype")))

        (setq num-buffer-words
            (count-words
            (point-min)
            (point-max)))
        (setq indices
            (sort
            (cl-loop for i from 0 below true-num-words
                collect
                (random (- num-buffer-words i)))
            '<))
        (setq res
            (cl-loop repeat true-num-words
                for idx in indices
                collect
                (progn
                (goto-char (point-min))
                (forward-word idx)
                (let ((word-to-return
                    (string-trim
                    (buffer-substring-no-properties
                    (point)
                    (progn (forward-word) (point))))))
                (kill-word -1)
                word-to-return)))))

        (with-current-buffer (get-buffer-create final-buffer)
        (erase-buffer)
        (insert (mapconcat 'identity res " ")))
        (switch-to-buffer final-buffer)
        (monkeytype-buffer)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;;; Miscellaneous                                                                   ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;; Better comment box                                                              ;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Last Edit                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun last-edit ()
  "Go back to last add/delete edit"
  (interactive)
  (let* ((ubuf (cadr buffer-undo-list))
         (beg (car ubuf))
         (end (cdr ubuf)))
    (cond
     ((integerp beg) (goto-char beg))
     ((stringp beg) (goto-char (abs end))
      (message "DEL-> %s" (substring-no-properties beg)))
     (t (message "No add/delete edit occurred")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Center scroll minor mode                                                        ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode prot/scroll-center-cursor-mode
  "Toggle centred cursor scrolling behavior"
  :init-value nil
  :lighter " S="
  :global nil
  (if prot/scroll-center-cursor-mode
      (setq-local scroll-margin (* (frame-height) 2)
                  scroll-conservatively 0
                  maximum-scroll-margin 0.5)
    (dolist (local '(scroll-preserve-screen-position
                     scroll-conservatively
                     maximum-scroll-margin
                     scroll-margin))
      (kill-local-variable `,local))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Theme functions                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; turn on dark theme
(defun go-dark-theme ()
  (interactive)
  (setq my-theme-shade "dark")
  (load-theme my-dark-theme))
  

;; turn on light theme
(defun go-light-theme ()
  (interactive)
  (setq my-theme-shade "light")
  (load-theme my-light-theme))
  

;; switch between light and dark theme
(defun switch-theme ()
  (interactive)
  (if (equal my-theme-shade "light")
      (go-dark-theme)
    (go-light-theme)))
