;;config.el -*- lexical-binding: t; -*-
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
(defvar my-keyboard-variant "qwerty")                 ; colemak or qwerty
(defvar my-dark-theme 'ef-night)
(defvar my-boon-default-cursor-color-dark "#00ccff")  ; boon cursor for ef-night
(defvar my-light-theme 'ef-day)
(defvar my-boon-default-cursor-color-light "#cf1f00") ; boon cursor for ef-day
(defvar my-main-theme my-dark-theme)
(defvar my-theme-shade "dark")                        ; can be light or dark. Used to color the Boon-mode cursor

(defvar my-monospace-font "Overpass Mono")            ; Font to use for code
(defvar my-variablespace-font "Overpass")             ; Font to use for writing

(defvar my-org-tracktable-daily-goal 2000)            ; How many words do I want to write per day?
(defvar my-line-spacing 28)                           ; how much space between the lines?
(defvar my-day-end 5)                                 ; when does my day end?

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
                             "~/habits/habits.org"))
                             
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Personal Information                                                             ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Jacob Moena"
      user-mail-address "jacmoe.dk@gmail.com")

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
(setq custom-file (make-temp-file "emacs-custom"))     ; prevent custom from preserving state
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
;; Remove all but the first menu entry on the splash screen
(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 1))
;; Set the title
(setq frame-title-format '("%b – Hotel California of Creative Writing"))
;; Add to the dashboard
(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Hotel California of Creative Writing")))

;; Fonts - ordinary and variable pitch
(setq doom-font (font-spec :family my-monospace-font :size 20)
      doom-big-font-increment 5
      doom-variable-pitch-font (font-spec :family my-variablespace-font :size 28))

;; Theme
(setq doom-theme my-main-theme)

;; Make the modified file name in the modeline orange instead of red
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

;; Because Emacs do not render italics at all when markup is hidden force it to render it differently
(custom-set-faces!
  '(italic :slant oblique :foreground "teal"))

;; Setting initial size and position of frame
(setq initial-frame-alist '((top . 38) (left . 76) (width . 130) (height . 38)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Global keybindings                                                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! "<f8>" #'dictionary-lookup-definition)     ; Look up word in the dictionary
(map! "<f9>" #'+zen/toggle)                      ; Toggle Zen mode
(map! "C-<f9>" #'writeroom-toggle-mode-line)     ; Toggle Zen mode-line
(map!"C-<down>" #'enlarge-window)
(map!"C-<up>" #'shrink-window)
(map!"C-<left>" #'enlarge-window-horizontally)
(map!"C-<right>" #'shrink-window-horizontally)
(map! "C-d" #'diff-buffer-with-file)             ; view what is modified
(map! "C-c t m" #'hide-mode-line-mode)           ; hide the mode-line
(map! "C-c t d" #'switch-theme)                  ; switch theme light/dark
(map! "C-c n q" #'tb/capture-to-this-buffer)     ; quick capture to this buffer
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
;; Flymake-proselint
;; Writegood-mode
;; Mw-thesaurus
;; Emacs-powerthesaurus
;; Browse-kill-ring
;; Dimmer
;; Yasnippet
;; Nov.el
;; Emacs-everywhere
;; Atomic-chrome
;; Engine-mode
;; EF-Themes
;; Mastodon
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
           #'writegood-mode
           #'variable-pitch-mode)

;; Org-habit
(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 35
        org-habit-show-habits t))
  

;; use :ignore: tags to ignore the heading, but keep the content
(use-package! ox-extra
  :after org
  :config
  (ox-extras-activate '(ignore-headlines)))
  

(add-hook! org-mode (org-indent-mode -1))

(after! org
  (custom-set-faces!
    '((org-block) :background nil))
    
  (defface redd
    '((((class color) (min-colors 88) (background light))
       :foreground "red"))
    "Red."
    :group 'basic-faces)
  (custom-set-faces!
    '(org-level-1 :height 1.2 :weight bold :slant normal)
    '(org-level-2 :height 1.1 :weight bold :slant normal)
    '(org-level-3 :height 1.0 :weight bold :slant normal)
    '(org-document-title   ;:foreground ,(doom-color 'black)
      :family my-variablespace-font
      :height 250
      :weight bold)))

(after! org
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
          
        ;; Don't pollute the text with markers
        org-hide-emphasis-markers t
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
           "* %?\n%t\n%i\n%a"))))
        

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
            (org-agenda-ndays 7)
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
  :defer t
  :after org-roam)

(use-package! org-roam-ui
  :defer t
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
   org-pomodoro-play-sounds nil ; don't play sounds
   org-pomodoro-length 20       ; 20 minutes are great for word-sprints
   org-pomodoro-short-break-length 5
   ;; use libnotify
   alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))
   

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
(setq save-abbrevs 'silently)     ; stop asking whether to save newly added abbrev when quitting emacs

(add-hook 'doom-first-buffer-hook ; one abbrev file for all modes
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))))

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
  (setq boon-insert-cursor-color "orange")
  (if (equal my-theme-shade "dark")
      (progn
        (setq boon-default-cursor-color "#00ccff"))
        
    (setq boon-default-cursor-color "#cf1f00"))
    
  (if (equal my-keyboard-variant "colemak")
      ;; we are using Colemak
      (progn
        ;; free keys
        (define-key boon-command-map "h" 'ignore)
        (define-key boon-command-map "k" 'ignore)
        (define-key boon-command-map "m" 'ignore)
        (define-key boon-command-map "æ" 'ignore)

        ;; Colemak row 1
        (define-key boon-command-map "q" '("quote" . boon-quote-character))
        (define-key boon-command-map "w" 'org-tracktable-status)
        (define-key boon-command-map "W" 'org-tracktable-write)
        (define-key boon-command-map "f" 'org-narrow-to-subtree)
        (define-key boon-command-map "F" 'widen)
        (define-key boon-command-map "p" '("paste" . boon-splice))
        (define-key boon-command-map "G" 'grab-x-link)
        (define-key boon-command-map "l" 'move-beginning-of-line)
        (define-key boon-moves-map "u"  'previous-line)
        (define-key boon-moves-map "y"  'next-line)
        (define-key boon-moves-map "U"  'backward-paragraph)
        (define-key boon-moves-map "Y"  'forward-paragraph)
        (define-key boon-command-map ";" 'move-end-of-line)
        (define-key boon-command-map "ø" 'move-end-of-line)
        ;; Colemak row 2
        (define-key boon-command-map "a" 'last-edit)
        (define-key boon-command-map "r" '("replace" . boon-substitute-region))
        (define-key boon-command-map "S" 'prot/scroll-center-cursor-mode)
        (define-key boon-command-map "t" '("transform" . boon-replace-by-character))
        (define-key boon-command-map "d" '("delete" . boon-take-region)) ; "delete"
        (define-key boon-command-map "D" 'boon-treasure-region) ; "duplicate"
        ;; h
        (define-key boon-moves-map "n"  'boon-smarter-backward)
        (define-key boon-moves-map "e"  'backward-char)
        (define-key boon-moves-map "i"  'forward-char)
        (define-key boon-command-map "E" 'backward-sentence)
        (define-key boon-command-map "I" 'forward-sentence)
        (define-key boon-command-map (kbd "C-e") 'scroll-down-line)
        (define-key boon-command-map (kbd "C-i") 'scroll-up-line)
        (define-key boon-moves-map "o"  'boon-smarter-forward)
        ;; Colemak row 3
        (define-key boon-command-map "Z" 'toggle-transparency)
        (define-key boon-command-map "x" 'boon-x-map)
        (define-key boon-command-map "c" 'boon-c-god)
        (define-key boon-command-map "v" '("v looks like an insert mark" . boon-set-insert-like-state))
        (define-key boon-command-map (kbd "C-v") 'boon-open-next-line-and-insert)
        (define-key boon-command-map "V" 'boon-open-line-and-insert)
        (define-key boon-command-map "B" 'org-pomodoro))
        ;; k
        ;; m
        
    ;; we are using Qwerty
    (progn
      ;; free keys
      (define-key boon-command-map "a" 'ignore)
      (define-key boon-command-map "f" 'ignore)
      (define-key boon-command-map "h" 'ignore)
      (define-key boon-command-map "m" 'ignore)

      ;; qwerty row 1
      (define-key boon-command-map "q" '("quote" . boon-quote-character))
      (define-key boon-command-map "w" 'org-tracktable-status)
      (define-key boon-command-map "W" 'org-tracktable-write)
      (define-key boon-command-map "e" 'last-edit)
      (define-key boon-command-map "r" '("replace" . boon-substitute-region))
      (define-key boon-command-map "t" '("transform" . boon-replace-by-character))
      (define-key boon-command-map "y" '("yank" . boon-splice))
      (define-key boon-command-map "u" 'move-beginning-of-line)
      (define-key boon-moves-map "i"  'previous-line)
      (define-key boon-moves-map "o"  'next-line)
      (define-key boon-moves-map "I"  'backward-paragraph)
      (define-key boon-moves-map "O"  'forward-paragraph)
      (define-key boon-command-map "p" 'move-end-of-line)
      ;; qwerty row 2
      ;; a
      (define-key boon-command-map "s" 'prot/scroll-center-cursor-mode)
      (define-key boon-command-map "d" '("delete" . boon-take-region)) ; "delete"
      (define-key boon-command-map "D" 'boon-treasure-region) ; "duplicate"
      ;; f
      (define-key boon-command-map "G" 'grab-x-link)
      ;; h
      (define-key boon-moves-map "j"  'boon-smarter-backward)
      (define-key boon-moves-map "k"  'backward-char)
      (define-key boon-moves-map "l"  'forward-char)
      (define-key boon-command-map "K" 'backward-sentence)
      (define-key boon-command-map "L" 'forward-sentence)
      (define-key boon-command-map (kbd "C-k") 'scroll-down-line)
      (define-key boon-command-map (kbd "C-l") 'scroll-up-line)
      (define-key boon-moves-map ";"  'boon-smarter-forward)
      (define-key boon-command-map "æ" 'boon-smarter-forward)
      ;; qwerty row 3
      (define-key boon-command-map "Z" 'toggle-transparency)
      (define-key boon-command-map "x" 'boon-x-map)
      (define-key boon-command-map "c" 'boon-c-god)
      (define-key boon-command-map "v" '("v looks like an insert mark" . boon-set-insert-like-state))
      (define-key boon-command-map (kbd "C-v") 'boon-open-next-line-and-insert)
      (define-key boon-command-map "V" 'boon-open-line-and-insert)
      (define-key boon-command-map "B" 'org-pomodoro)
      (define-key boon-command-map "n" 'org-narrow-to-subtree)
      (define-key boon-command-map "N" 'widen)))
      ;; m
      
    

  (define-key boon-moves-map "<"  'beginning-of-buffer)
  (define-key boon-moves-map ">"  'end-of-buffer)

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
;; Flymake-proselint                                                                ;;
;;                                                                                  ;;
;; https://github.com/manuel-uberti/flymake-proselint                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! flymake-proselint
  :defer t
  :init
  (add-hook! #'org-mode-hook (lambda ()
                               (flymake-mode)
                               (flymake-proselint-setup))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Writegood-mode                                                                   ;;
;;                                                                                  ;;
;; https://github.com/bnbeckwith/writegood-mode                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! writegood-mode
  :config
 (setq my/weasel-words
       '("actually"
         "basically"
         "easily"
         "easy"
         "simple"
         "simply"))
 (setq writegood-weasel-words
       (-concat writegood-weasel-words my/weasel-words)))
  

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
;; EF-Themes                                                                        ;;
;;                                                                                  ;;
;; https://github.com/prot/ef-themes                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! ef-themes
  (setq ef-themes-mixed-fonts 1)
  (ef-themes--load-theme my-main-theme)
  (turn-off-boon-mode)
  (turn-on-boon-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Mastodon                                                                         ;;
;;                                                                                  ;;
;; https://codeberg.org/martianh/mastodon.el                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mastodon-instance-url "https://writing.exchange"
      mastodon-active-user "@jacmoe"
      mastodon-toot--download-custom-emoji t
      mastodon-toot--enable-custom-emoji t
      mastodon-toot--enable-completion t)

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
;; Layout switching                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; switch to Danish keyboard layout
(defun my/kbdk ()
  (interactive)
  (if (equal my-keyboard-variant "colemak")
      ;; we are using Colemak
      (progn
        (call-process-shell-command "setxkbmap" nil nil nil "-layout no -variant colemak")
        ;; (call-process-shell-command "xmodmap" nil nil nil "-e \"keycode 32 = j J\"")
        ;; (call-process-shell-command "xmodmap" nil nil nil "-e \"keycode 29 = y Y\"")
        (message "Norwegian Colemak"))
        
    (progn
      (call-process-shell-command "setxkbmap" nil nil nil "dk")
      (message "Danish Qwerty"))))
      
  
(map! "C-c k" #'my/kbdk)

;; switch to American keyboard layout
(defun my/kbus ()
  (interactive)
  (if (equal my-keyboard-variant "colemak")
      ;; we are using Colemak
      (progn
        (call-process-shell-command "setxkbmap" nil nil nil "-layout us -variant colemak")
        (message "US Colemak"))
        
    (progn
      (call-process-shell-command "setxkbmap" nil nil nil "us")
      (message "US Qwerty"))))
      
  
(map! "C-c u" #'my/kbus)

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
  (setq boon-default-cursor-color my-boon-default-cursor-color-dark)
  (setq my-theme-shade "dark")
  (ef-themes--load-theme my-dark-theme))
  

;; turn on light theme
(defun go-light-theme ()
  (interactive)
  (setq boon-default-cursor-color my-boon-default-cursor-color-light)

  (setq my-theme-shade "light")
  (ef-themes--load-theme my-light-theme))
  

;; switch between light and dark theme
(defun switch-theme ()
  (interactive)
  (if (equal my-theme-shade "light")
      (go-dark-theme)
    (go-light-theme)))
    
  
