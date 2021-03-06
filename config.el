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
(defvar my-dark-theme 'catppuccin)         ; catppuccin, misterioso, uwu
(defvar my-light-theme 'tsdh-light)
(defvar my-main-theme my-dark-theme)
(defvar my-theme-shade "dark")             ; can be light or dark. Used to color the Boon-mode cursor
(defvar my-org-tracktable-daily-goal 1000) ; How many words do I want to write per day?
(defvar my-line-spacing 8)                 ; how much space between the lines?
(defvar my-day-end 5)                      ; when does my day end?
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
                             "~/habits/habits.org"
                             )
      )

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
(setq secret-file (expand-file-name ".secret.el" doom-private-dir))
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
;; Mouse-avoidance makes the frame "jump" on Windows...
(unless (eq system-type 'windows-nt)
  (if (display-mouse-p) (mouse-avoidance-mode 'banish)))  ; Shove the mouse pointer out of  the way
;; tell Undo-fu to only store linear history, not the full history
(setq undo-fu-session-linear t)
(map! "C-;" nil)                                       ; Don't steal my C-; !

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Visual settings                                                                  ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convince Emacs to trust themes so that we can install them
(setq custom-safe-themes t)
;; Fancy splash screen
(setq fancy-splash-image (expand-file-name "splash/emacs.png" doom-private-dir))
;; Remove all but the first menu entry on the splash screen
(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 1))
;; Set the title
(setq frame-title-format '("%b ??? Hotel California of Creative Writing"))
;; Add to the dashboard
(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Hotel California of Creative Writing")))

;; Fonts - ordinary and variable pitch
(setq doom-font (font-spec :family "Overpass Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "Alegreya" :size 28))

;; Theme
(setq doom-theme my-main-theme)

;; Make the modified file name in the modeline orange instead of red
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

;; Setting initial size and position of frame
;; It is a necessary hack because Doom doesn't seem to
;; care about my frame size when restoring sessions ...
(if (eq system-type 'windows-nt)
    (setq initial-frame-alist '((top . 38) (left . 66) (width . 124) (height . 32)))
  (setq initial-frame-alist '((top . 38) (left . 76) (width . 130) (height . 38)))
  )

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
(map! "<f9>" #'doom-big-font-mode)   ; Toggle big font mode
(map!"C-<down>" #'enlarge-window)
(map!"C-<up>" #'shrink-window)
(map!"C-<left>" #'enlarge-window-horizontally)
(map!"C-<right>" #'shrink-window-horizontally)
(map! "C-`" #'diff-buffer-with-file) ; view what is modified
(map! "C-c t m" #'hide-mode-line-mode) ; hide the mode-line
(map! "C-c t d" #'switch-theme) ; switch theme light/dark
(map! "C-c n q" #'tb/capture-to-this-buffer) ; quick capture to this buffer

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
(defalias 'wwg-goal 'wwg/set-goal-current-buffer)
(defalias 'wwg-1k 'wwg/set-1k-goal-current-buffer)
(defalias 'whmode 'writing-header-line-mode)

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
;; Org-super-agenda
;; Org-super-links
;; Org-transclusion
;; Citar
;; Ox-Hugo
;; Orgdiff
;; Org-journal
;; Annotate
;; ISpell
;; Abbrev
;; Typopunct
;; Smart-quotes
;; Boon
;; CtrlF
;; Transparency
;; Move-text
;; Flymake-proselint
;; Smog
;; Mw-thesaurus
;; Emacs-powerthesaurus
;; Lexic
;; Wwg - Writer Word Goals
;; Browse-kill-ring
;; Dimmer
;; Yasnippet
;; Nov.el
;; Palimpsest
;; Emacs-everywhere
;; Atomic-chrome
;; Engine-mode
;; Miscellaneous

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-mode                                                                         ;;
;;                                                                                  ;;
;; https://orgmode.org/                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A GNU Emacs major mode for keeping notes, authoring documents, computational notebooks,
;; literate programming, maintaining to-do lists, planning projects,
;; and more ??? in a fast and effective plain text system.

(add-hook! org-mode :append
           #'visual-line-mode
           #'solaire-mode
           #'typopunct-mode
           #'variable-pitch-mode)

;; Org-habit
(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 35
        org-habit-show-habits t)
  )

;; use :ignore: tags to ignore the heading, but keep the content
(use-package! ox-extra
  :after org
  :config
  (ox-extras-activate '(ignore-headlines))
  )

(add-hook! org-mode (org-indent-mode -1))

(after! org
  (custom-set-faces!
    '((org-block) :background nil)
    )
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
      :family "ETBembo"
      :height 250
      :weight bold)))

(after! org
  (setq org-startup-folded 'show2levels
        org-enforce-todo-dependencies t
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
          )
        ;; Don't pollute the text with markers
        org-hide-emphasis-markers t
        org-return-follows-link t ; hitting RETURN follows the link
        ;; We want to log the time when the TODO is closed
        org-log-done "time" org-log-done-with-time 't
        ;; If idle for more than 10 minutes, resolve the things
        ;; by asking what to do with the clock time
        org-clock-idle-time 10
        org-latex-toc-command "\\tableofcontents \\clearpage" ; Force page break after TOC for PDF exports
        org-latex-image-default-width ""
        org-latex-image-default-scale "0.4"
        org-latex-images-centered nil
        org-latex-toc-command "\\clearpage \\tableofcontents \\clearpage"
        org-fontify-done-headline nil ; don't color the headline grey when done
        org-capture-templates
        '(("s" "Slipbox" entry  (file "inbox.org")
           "* %?\n%t\n%i\n%a"))
        ))

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
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":daily:"))))
          ))
  )

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
   alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-super-agenda                                                                 ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-super-agenda
  :defer t
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due today"
                                   :deadline today)
                                  (:name "Important"
                                   :priority "A")
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Due soon"
                                   :deadline future)))
  :config
  (org-super-agenda-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-super-links                                                                  ;;
;;                                                                                  ;;
;; https://github.com/toshism/org-super-links                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-super-links
  :defer t
  :after org
  :config
  (map! :map org-mode-map
        :localleader
        :prefix ("S" . "org-super-links")
        "l" #'org-super-links-link
        "s" #'org-super-links-store-link
        "i" #'org-super-links-insert-link
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-transclusion                                                                 ;;
;;                                                                                  ;;
;; https://github.com/nobiot/org-transclusion                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-transclusion
  :defer t
  :after org)

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
   citar-notes-paths (list org-roam-directory))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Ox-Hugo                                                                          ;;
;;                                                                                  ;;
;; https://ox-hugo.scripter.co/                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org exporter backend that exports Org to Hugo-compatible Markdown
(after! ox-hugo
  (plist-put org-hugo-citations-plist :bibliography-section-heading "Bibliography")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Orgdiff                                                                          ;;
;;                                                                                  ;;
;; https://github.com/tecosaur/orgdiff                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package! orgdiff
;;   :config
;;   (defun +orgdiff-nicer-change-colours ()
;;     (goto-char (point-min))
;;     ;; Set red/blue based on whether chameleon is being used
;;     (if (search-forward "%% make document follow Emacs theme" nil t)
;;         (setq red  (substring (doom-blend 'red 'fg 0.8) 1)
;;               blue (substring (doom-blend 'blue 'teal 0.6) 1))
;;       (setq red  "c82829"
;;             blue "00618a"))
;;     (when (and (search-forward "%DIF PREAMBLE EXTENSION ADDED BY LATEXDIFF" nil t)
;;                (search-forward "\\RequirePackage{color}" nil t))
;;       (when (re-search-forward "definecolor{red}{rgb}{1,0,0}" (cdr (bounds-of-thing-at-point 'line)) t)
;;         (replace-match (format "definecolor{red}{HTML}{%s}" red)))
;;       (when (re-search-forward "definecolor{blue}{rgb}{0,0,1}" (cdr (bounds-of-thing-at-point 'line)) t)
;;         (replace-match (format "definecolor{blue}{HTML}{%s}" blue)))))
;;   (add-to-list 'orgdiff-latexdiff-postprocess-hooks #'+orgdiff-nicer-change-colours))

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
  (setq annotate-database-confirm-deletion t)
  )

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
  (require 'boon-qwerty)
  (require 'boon-tutorial)
  :config
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
  (define-key boon-command-map "s" 'prot/scroll-center-cursor-mode)
  (define-key boon-command-map "n" 'org-narrow-to-subtree)
  (define-key boon-command-map "N" 'widen)
  (define-key boon-command-map "M" 'doom-modeline-mode)
  (define-key boon-command-map "w" 'org-tracktable-status)
  (define-key boon-command-map "??" 'boon-smarter-forward)
  (add-hook 'lexic-mode-hook 'turn-off-boon-mode)
  (add-hook 'ibuffer-hook 'turn-off-boon-mode)
  (add-hook 'doom-dashboard-mode 'turn-off-boon-mode)
  (add-hook 'org-capture-mode-hook 'turn-off-boon-mode)
  :bind
  ("<f6>" . turn-on-boon-mode)
  ("<f7>" . turn-off-boon-mode)
  ("C-;" . boon-set-command-state); used to quit insert mode
  ("C-??" . boon-set-command-state); used to quit insert mode - Danish version
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
                               (flymake-proselint-setup)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Smog                                                                             ;;
;;                                                                                  ;;
;; https://github.com/zzkt/smog                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smog means "Simple Measure Of Gobbledygook". Checks buffer for readability
;; Prerequisite: apt install diction on Ubunbu
(use-package smog
  :defer t
  :config (setq smog-command "style -L en"))

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
      (setq mw-thesaurus--api-key my-mw-api-key))
  )

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
  ("S-<f5>" . powerthesaurus-lookup-antonyms-dwim)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Lexic                                                                            ;;
;;                                                                                  ;;
;; https://github.com/tecosaur/lexic                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! lexic
  :defer t
  :bind
  ("<f8>" . +lookup/dictionary-definition-lexic)
  ("S-<f8>" . lexic-search)
  )

(defadvice! +lookup/dictionary-definition-lexic (identifier &optional
                                                            arg) "Look up the definition of the word at point (or selection) using
 `lexic-search'."
                                                            :override #'+lookup/dictionary-definition
                                                            (interactive
                                                             (list (or (doom-thing-at-point-or-region 'word)
                                                                       (read-string "Look up in dictionary: "))
                                                                   current-prefix-arg))
                                                            (lexic-search identifier nil nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Wwg - Writer Word Goals                                                          ;;
;;                                                                                  ;;
;; https://github.com/ag91/writer-word-goals                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! wwg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Browse-kill-ring                                                                 ;;
;;                                                                                  ;;
;; https://github.com/browse-kill-ring/browse-kill-ring                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! browse-kill-ring
  :defer t
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
;; Palimpsest                                                                       ;;
;;                                                                                  ;;
;; https://github.com/danielsz/Palimpsest                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This minor mode for Emacs provides several strategies to remove text without permanently deleting it.
(use-package! palimpsest
  :defer t
  :config
  (setq palimpsest-prefix "Boneyard")
  )

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
;; Miscellaneous                                                                    ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch to Danish keyboard layout
(defun my/kbdk ()
  (interactive)
  (call-process-shell-command "setxkbmap" nil nil nil "dk")
  (message "Danish keyboard layout")
  )
(map! "C-c k" #'my/kbdk)

;; switch to American keyboard layout
(defun my/kbus ()
  (interactive)
  (call-process-shell-command "setxkbmap" nil nil nil "us")
  (message "US keyboard layout")
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

;; center scroll minor mode
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
      (kill-local-variable `,local)))
  )


;; Writing header mode-line minor mode
;; A bug causes the doom mode-line to disappear when this
;; minor mode is toggled off.
;; Run M-x doom-modeline-mode twice to fix the missing modeline
(defvar writing-header--default-format header-line-format
  "Storage for the default `mode-line-format'.
So it can be restored when 'writer-header-line-mode' is disabled.")

(defvar writing-modeline--default-format mode-line-format)

(define-minor-mode writing-header-line-mode
  "Adds a bar with the same color as the fringe as the header-line.
Imitates the look of wordprocessors a bit."
  :init-value nil
  :global nil
  (if writing-header-line-mode
      (progn
        (setq header-line-format
              (concat
               (propertize " " 'display (list 'space :width 'left-fringe) 'face 'fringe)
               (propertize " " 'display (list 'space :width 'left-margin) 'face (list (list :height 400) 'default))
               (propertize " " 'display (list 'space :width 'text) 'face (list (list :height 400) 'default))
               (propertize " " 'display (list 'space :width 'left-margin) 'face (list (list :height 400) 'default))
               (propertize " " 'display (list 'space :width 'left-fringe) 'face 'fringe))) ;
        (setq mode-line-format header-line-format))
    (setq header-line-format writing-header--default-format
          mode-line-format writing-modeline--default-format)))

;; turn on dark theme
(defun go-dark-theme ()
  (interactive)
  (setq boon-default-cursor-color "white")
  (setq my-theme-shade "dark")
  (load-theme my-dark-theme t)
  )

;; turn on light theme
(defun go-light-theme ()
  (interactive)
  (setq boon-default-cursor-color "black")
  (setq my-theme-shade "light")
  (load-theme my-light-theme)
  )

;; turn on poet theme
(defun poet-theme ()
  (interactive)
  (setq boon-default-cursor-color "black")
  (setq my-theme-shade "light")
  (load-theme 'poet t)
  )

;; turn on typo theme
(defun typo-theme ()
  (interactive)
  (setq boon-default-cursor-color "black")
  (setq my-theme-shade "light")
  (load-theme 'typo t)
  )

;; switch between light and dark theme
(defun switch-theme ()
  (interactive)
  (if (equal my-theme-shade "light")
      (go-dark-theme)
    (go-light-theme)
    )
  )
