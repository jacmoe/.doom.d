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
;; Variables                                                                        ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-keyboard-variant "qwerty")                 ; colemak or qwerty

;; (defvar my-dark-theme 'poet-dark)
(defvar my-light-theme 'poet)
(defvar my-dark-theme 'doom-xcode)
;; (defvar my-light-theme 'doom-plain)

(defvar my-main-theme my-dark-theme)
(defvar my-theme-shade "dark")                       ; can be light or dark.

(defvar my-monospace-font "Overpass Mono")      ; Font to use for code
(defvar my-variablespace-font "Carlito")             ; Font to use for writing ;Carlito;Alegreya
(defvar my-monospace-font-size 26)
(defvar my-variablespace-font-size 36)

(defvar my-org-tracktable-daily-goal 500)            ; How many words do I want to write per day?
(defvar my-line-spacing 12)                          ; how much space between the lines?
(defvar my-day-end 5)                                ; when does my day end?

(defvar my-org-pomodoro-play-sounds nil) ; don't play sounds
(defvar my-org-pomodoro-length 20)       ; 20 minutes are great for word-sprints
(defvar my-org-pomodoro-short-break-length 5)


;; Where do I store everything to be shared between machines?
(defvar my-storage-directory "~/Dropbox/skriv/")
(defvar my-personal-dictionary (concat my-storage-directory "aspell-en")) ; store personal dictionary here
(setq bookmark-default-file (concat my-storage-directory "bookmarks")) ; Where to save the bookmarks file
(setq annotate-file (concat my-storage-directory "annotations"))
(setq bib-file (concat my-storage-directory "jacmoe.bib"))
(defvar my-journal-directory (concat my-storage-directory "journal/"))

;; Org Directories
(setq +org-roam-auto-backlinks-buffer t
      org-directory (concat my-storage-directory "org/")
      org-roam-directory org-directory
      org-roam-dailies-directory (concat org-directory "roam-journal/")
      org-archive-location (concat org-directory ".archive/%s::")
      org-agenda-files (list org-directory
                             "~/enestaaende/enestaaende.org"
                             "~/Dropbox/skriv/habits/habits.org"))
                             
(defvar my-notmuch-address-command "/home/moena/notmuch-addrlookup-c/notmuch-addrlookup")
(defvar my-puppeteer-config-file "/home/moena/puppeteerConfigFile.json")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Personal Information                                                             ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Jacob Moena"
      user-mail-address "jacmoe.dk@gmail.com"
      user-mail-domain "gmail.com"
      user-instance-url "https://writing.exchange"
      mastodon-username "@jacmoe"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Secret Information                                                               ;;
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
;; General settings                                                                 ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(save-place-mode 1)                                    ; Remember and restore the last cursor location of opened files
(setq confirm-kill-emacs nil)                          ; Yes, I really want to quit.
(setq inhibit-compacting-font-caches t)                ; for performance reasons
(setq bookmark-save-flag 1)                            ; Save bookmarks each time it changes, not only on exit
(require 'zone)                                        ; Emacs "screensaver"
(zone-when-idle 300)                                   ; Zone out when idle for five minutes.
(setq enable-local-eval t)                             ; Define safe local variables
(unless (eq system-type 'windows-nt)                   ; Mouse-avoidance makes the frame "jump" on Windows...
  (if (display-mouse-p) (mouse-avoidance-mode 'jump)))  ; Shove the mouse pointer out of  the way
(setq undo-fu-session-linear t)                        ; tell Undo-fu to only store linear history, not the full history
(map! "C-;" nil)                                       ; Don't steal my C-; !
(setq emojify-download-emojis-p t)                     ; Force Doom-Emacs to download emojis without asking

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Visual settings                                                                  ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convince Emacs to trust themes so that we can install them
(setq custom-safe-themes t)
;; Fancy splash screen
(setq fancy-splash-image (expand-file-name "splash/emacs.png" doom-user-dir))

(setq +doom-dashboard-menu-sections
  '(
    ("View open buffers"
    :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
    :action ibuffer)
    ("Jump to bookmark"
    :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
    :action bookmark-jump)
    ("Recently opened files"
    :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
    :action recentf-open-files)
    ("Org-journal"
    :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
    :action org-journal-new-entry)
    ("Org-roam"
    :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
    :action org-roam-node-find)
    ("Open private configuration"
    :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
    :when (file-directory-p doom-private-dir)
    :action doom/open-private-config)))

;; Set the title
(setq frame-title-format '("%b – Hotel California of Creative Writing"))
;; Add to the dashboard
(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Hotel California of Creative Writing")))

;; Fonts - ordinary and variable pitch
(setq doom-font (font-spec :family my-monospace-font :size my-monospace-font-size :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family my-variablespace-font :size my-variablespace-font-size))

;; Theme
(setq doom-theme my-main-theme)

;; Make the modified file name in the modeline orange instead of red
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

;; Make the Doom modeline occupy a more comfortable amount of space
(setq doom-modeline-height 45)

;; Because Emacs do not render italics at all when markup is hidden force it to render it differently
(custom-set-faces!
  '(italic :slant oblique :foreground "teal"))

(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font
    t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

;; Misc settings
(setq display-line-numbers-type nil)                        ; Do not show line numbers
(display-time-mode 1)                                       ; Display time in modeline
(setq display-time-format "%H:%M")                          ; Set format of clock (Hours:Minutes)
(setq display-time-24hr-format 1)                           ; Clock is using 24 hour format
(fringe-mode '(80 . 80))                                    ; Show vertical fringes
(blink-cursor-mode t)                                       ; The cursor should blink
(setq-default line-spacing my-line-spacing)                 ; The amount of space between lines in pixels
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode) ; Do not highlight current line
(setq global-page-break-lines-mode t)                       ; Pretty page breaks everywhere
(setq confirm-kill-processes nil)                           ; Don't ask to kill running processes when exiting Emacs.
(setq +notmuch-sync-backend 'offlineimap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Global keybindings                                                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :desc "Show TrackTable status" "C-c h w" #'org-tracktable-status)
(map! :desc "Write to TrackTable" "C-c h W" #'org-tracktable-write)
(map! :desc "Grab link from browser" "C-c h g" #'grab-x-link)
(map! :desc "Go to last edit" "C-c h e" #'last-edit)
(map! :desc "Center cursor mode" "C-c h s" #'prot/scroll-center-cursor-mode)
(map! :desc "Toggle transparency" "C-c h t" #'toggle-transparency)
(map! :desc "Start Pomodoro" "C-c h b" #'org-pomodoro)

(map! :desc "Look up word in dictionary" "<f8>" #'dictionary-lookup-definition)     ; Look up word in the dictionary
(map! :desc "Toggle mode-line" "C-<f9>" #'hide-mode-line-mode)             ; Toggle mode-line
(map! :desc "Enlarge window" "C-<down>" #'enlarge-window)
(map! :desc "Shrink window" "C-<up>" #'shrink-window)
(map! :desc "Enlarge window horizontally" "C-<left>" #'enlarge-window-horizontally)
(map! :desc "Shrink window horizontally" "C-<right>" #'shrink-window-horizontally)
;; (map! "C-d" #'diff-buffer-with-file)             ; view what is modified
(map! :desc "Toggle mode-line" "C-c t m" #'hide-mode-line-mode)           ; hide the mode-line
(map! :desc "Toggle light/dark theme" "C-c t d" #'switch-theme)                  ; switch theme light/dark
(map! :desc "Capture to this buffer" "C-c n q" #'tb/capture-to-this-buffer)     ; quick capture to this buffer
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
;; Keybindings defined elsewhere:                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (map! "C-c t s" #'toggle-transparency)
;; (map! "C-c k" #'my/kbdk)
;; (map! "C-c u" #'my/kbus)
;; ("<f6>" . turn-on-boon-mode)
;; ("<f7>" . turn-off-boon-mode)
;; ("<f5>" . powerthesaurus-lookup-synonyms-dwim)
;; ("S-<f5>" . powerthesaurus-lookup-antonyms-dwim)
;; ("C-c s n" . yas-new-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Aliases                                                                          ;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Features                                                                         ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;; Org-Roam
;; Org-Roam-UI
;; Org-tracktable
;; Org-appear
;; Org-pomodoro
;; Org-habit-stats
;; Citar
;; Ox-Hugo
;; Org-journal
;; Annotate
;; ISpell
;; Abbrev
;; Typopunct
;; Smart-quotes
;; Boon
;; CtrlF
;; Transparency
;; Flymake-vale
;; Mw-thesaurus
;; Emacs-powerthesaurus
;; Browse-kill-ring
;; Dimmer
;; Yasnippet
;; Nov.el
;; Emacs-everywhere
;; Atomic-chrome
;; Engine-mode
;; Mastodon
;; Smtpmail
;; Keycast
;; Gif-screencast
;; Olivetti-mode
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

(add-hook! org-mode :append
           #'visual-line-mode
           #'solaire-mode
           #'typopunct-mode
           #'flymake-vale-load
           #'mixed-pitch-mode
           #'solaire-mode)

(add-hook! 'mixed-pitch-mode-hook #'solaire-mode-reset)

;; Org-habit
(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 35
        org-habit-show-habits-only-for-today nil
        org-habit-show-habits t))
  
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-day nil ;; i.e. today
      org-agenda-span 1
      org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "To refile"
                                   :file-path "refile\\.org")
                            (:name "Next to do"
                                   :todo "NEXT"
                                   :order 1)
                            (:name "Important"
                                   :priority "A"
                                   :order 6)
                            (:name "Today's tasks"
                                   :file-path "journal/")
                            (:name "Due Today"
                                   :deadline today
                                   :order 2)
                            (:name "Scheduled Soon"
                                   :scheduled future
                                   :order 8)
                            (:name "Overdue"
                                   :deadline past
                                   :order 7)
                            (:name "Meetings"
                                   :and (:todo "MEET" :scheduled future)
                                   :order 10)
                            (:discard (:not (:todo "TODO")))))))))))
  :config
  (org-super-agenda-mode))

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
           "* %?\n%t\n%i\n%a")))
)
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
                     "** %?\n%t\n %a"))))
            (org-capture nil "n")))))

(defun jethro/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-Roam                                                                         ;;
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
;; Org-Roam-UI                                                                      ;;
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
;; Org-tracktable                                                                   ;;
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
;; Org-appear                                                                       ;;
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
;; Org-pomodoro                                                                     ;;
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
;; Org-habit-stats                                                                  ;;
;;                                                                                  ;;
;; https://github.com/ml729/org-habit-stats/                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-habit-stats
:after org
:init
(add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Citar                                                                            ;;
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
;; Ox-Hugo                                                                          ;;
;;                                                                                  ;;
;; https://ox-hugo.scripter.co/                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org exporter backend that exports Org to Hugo-compatible Markdown
(after! ox-hugo
  (plist-put org-hugo-citations-plist :bibliography-section-heading "Bibliography"))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-journal                                                                      ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-journal
  :defer t
  :after org
  :config
  (setq org-journal-dir my-journal-directory
        org-journal-file-format "%Y%m%d.org"
        org-journal-file-type 'monthly
        org-extend-today-until my-day-end
        org-journal-date-format "%A, %d %B %Y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Annotate                                                                         ;;
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
;; ispell                                                                           ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive spelling
(use-package! ispell
  :defer t
  :config
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
;; Abbrev                                                                           ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-text for Emacs
(setq save-abbrevs 'silently)   ; stop asking whether to save newly added abbrev when quitting emacs
(setq abbrev-file-name (expand-file-name "abbrev_defs.el" doom-private-dir)) ; where to load abbrevs from
(setq-default abbrev-mode t)    ; Abbrev mode always on

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Typopunct                                                                        ;;
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
;; Smart-quotes                                                                     ;;
;;                                                                                  ;;
;; https://github.com/gareth-rees/smart-quotes                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only used for the "smart-quotes-smarten" function
(use-package! smart-quotes
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Boon                                                                             ;;
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
   (add-hook 'speed-type-mode-hook 'turn-off-boon-mode)
   :bind
   ("<f6>" . turn-on-boon-mode)
   ("<f7>" . turn-off-boon-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; CtrlF                                                                            ;;
;;                                                                                  ;;
;; https://github.com/raxod502/ctrlf                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! ctrlf
  :config
  (ctrlf-mode t))


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
         '(86 . 50) '(100 . 100)))))

(map! "C-c t s" #'toggle-transparency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Flymake-vale                                                                     ;;
;;                                                                                  ;;
;; https://github.com/tpeacock19/flymake-vale                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! flymake-vale)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Mw-thesaurus                                                                     ;;
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
;; Emacs-powerthesaurus                                                             ;;
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
;; Browse-kill-ring                                                                 ;;
;;                                                                                  ;;
;; https://github.com/browse-kill-ring/browse-kill-ring                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! browse-kill-ring
  :defer t
  :bind
  ("M-y" . 'browse-kill-ring))
  

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
;; https://github.com/joaotavora/yasnippet                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! yasnippet
  :defer t
  :bind
  ("C-c s n" . yas-new-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Nov.el                                                                           ;;
;;                                                                                  ;;
;; https://depp.brause.cc/nov.el/                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! nov
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Emacs-everywhere                                                                 ;;
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
;; Atomic-chrome                                                                    ;;
;;                                                                                  ;;
;; https://github.com/alpha22jp/atomic-chrome                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! atomic-chrome
 :defer t
 :init
 (add-transient-hook! 'focus-out-hook (atomic-chrome-start-server)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Engine-mode                                                                      ;;
;;                                                                                  ;;
;; https://github.com/hrs/engine-mode                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! engine-mode
 :init
 (engine-mode t)
 :config
 (defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")
 (defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Mastodon                                                                         ;;
;;                                                                                  ;;
;; https://codeberg.org/martianh/mastodon.el                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mastodon-instance-url user-instance-url
      mastodon-active-user mastodon-username
      mastodon-toot--download-custom-emoji t
      mastodon-toot--enable-custom-emoji t
      mastodon-toot--enable-completion t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Smtpmail                                                                         ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mail-host-address user-mail-domain)
(setq user-full-name user-full-name)
(setq user-mail-adress user-mail-address)
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-kill-buffer-on-exit t)
(setq mail-specify-envelope-from t)
(setq sendmail-program "/usr/bin/msmtp"
  mail-specify-envelope-from t
  mail-envelope-from 'header
  message-sendmail-envelope-from 'header)

(require 'notmuch-address)
(setq notmuch-address-command my-notmuch-address-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Keycast                                                                          ;;
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
;; Gif-screencast                                                                   ;;
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
;; Olivetti-mode                                                                    ;;
;;                                                                                  ;;
;; https://github.com/rnkn/olivetti                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Distraction-free screen
(use-package! olivetti
  :init
  (setq olivetti-body-width .67)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("<f9>" . distraction-free)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Miscellaneous                                                                    ;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last Edit                                                                        ;;
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
;; center scroll minor mode                                                         ;;
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
;; Theme functions                                                                  ;;
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
