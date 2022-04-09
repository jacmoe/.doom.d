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
(defvar my-theme-shade "dark") ; can be light or dark. Used to color the Boon-mode cursor
;; (defvar my-theme-shade "light") ; can be light or dark. Used to color the Boon-mode cursor
(defvar my-org-tracktable-daily-goal 1000) ; How many words do I want to write per day?
(defvar my-line-spacing 8) ; how much space between the lines?
(defvar my-personal-dictionary "~/Dropbox/skriv/aspell-en") ; store personal dictionary here
(setq bookmark-default-file "~/Dropbox/skriv/bookmarks") ; Where to save the bookmarks file
;; Org Directories
(setq +org-roam-auto-backlinks-buffer t
      org-directory "~/org/"
      org-roam-directory org-directory
      org-roam-dailies-directory (concat org-directory "journal/")
      ;; org-archive-location (concat org-directory ".archive/%s::")
      org-agenda-files (list "~/org/"
                             ;; "~/Dropbox/skriv/"
                             "~/enestaaende/enestaaende.org"
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
(if (display-mouse-p) (mouse-avoidance-mode 'banish))  ; Shove the mouse pointer out of  the way
(after! gcmh
  (setq gcmh-high-cons-threshold 33554432))  ; 32mb, or 64mb, or *maybe* 128mb, BUT NOT 512mb

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
(setq frame-title-format '("%b – Hotel California of Creative Writing"))

;; Fonts - ordinary and variable pitch
(if (eq system-type 'windows-nt)
    (setq doom-font (font-spec :family "Overpass Mono" :size 20) ; Windows font
          doom-variable-pitch-font (font-spec :family "Alegreya" :size 32))
  (setq doom-font (font-spec :family "Overpass Mono" :size 24) ; Linux font
        doom-variable-pitch-font (font-spec :family "Alegreya" :size 32))
  )

;; Theme
(setq doom-theme 'misterioso)

;; Make the modified file name in the modeline orange instead of red
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

;; Setting initial size and position of frame
;; It is a necessary hack because Doom doesn't seem to
;; care about my frame size when restoring sessions ...
(if (eq system-type 'windows-nt)
    (setq initial-frame-alist '((top . 45) (left . 76) (width . 120) (height . 45)))
  (setq initial-frame-alist '((top . 45) (left . 76) (width . 90) (height . 30)))
  )

;; Misc settings
(setq display-line-numbers-type nil)                        ; Do not show line numbers
(display-time-mode 1)                                       ; Display time in modeline
(fringe-mode '(160 . 160))                                  ; Show vertical fringes
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
;; Org-transclusion
;; Orgdiff
;; Annotate
;; ISpell
;; Abbrev
;; Typopunct
;; Smart-quotes
;; Boon
;; CtrlF
;; Back-button
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
;; Emacs-everywhere
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
        ;; Don't pollute the text with markers
        org-hide-emphasis-markers t
        org-return-follows-link t ; hitting RETURN follows the link
        ;; We want to log the time when the TODO is closed
        org-log-done "time" org-log-done-with-time 't
        ;; If idle for more than 10 minutes, resolve the things
        ;; by asking what to do with the clock time
        org-clock-idle-time 10
        org-latex-toc-command "\\tableofcontents \\clearpage" ; Force page break after TOC for PDF exports
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
            (counter (+ org-habit-graph-column (- org-habit-preceding-days org-habit-following-days)))
            )
        (move-to-column counter)
        ;;until end of line
        (while (= (char-after (point)) org-habit-completed-glyph)
          (setq streak (+ streak 1))
          (setq counter (- counter 1))
          (backward-char 1))
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
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
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
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
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
  :config
  (setq org-tracktable-daily-goal my-org-tracktable-daily-goal))

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
;; Org-pomodoro                                                                     ;;
;;                                                                                  ;;
;; https://github.com/marcinkoziej/org-pomodoro                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-pomodoro
  :config
  (setq
   org-pomodoro-length 20
   org-pomodoro-short-break-length 5
alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Org-super-agenda                                                                 ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-super-agenda
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
;; Org-transclusion                                                                 ;;
;;                                                                                  ;;
;; https://github.com/nobiot/org-transclusion                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-transclusion)

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
(use-package! smart-quotes)

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
  (add-hook 'lexic-mode-hook 'turn-off-boon-mode)
  (add-hook 'ibuffer-hook 'turn-off-boon-mode)
  (add-hook 'doom-dashboard-mode 'turn-off-boon-mode)
  (add-hook 'org-capture-mode-hook 'turn-off-boon-mode)
  :bind
  ("<f6>" . turn-on-boon-mode)
  ("<f7>" . turn-off-boon-mode)
  ("C-;" . boon-set-command-state); used to quit insert mode
  ("C-æ" . boon-set-command-state); used to quit insert mode - Danish version
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
;; Back-button                                                                      ;;
;;                                                                                  ;;
;; https://github.com/rolandwalker/back-button                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-button provides an alternative method for navigation by analogy with the "back" button in a web browser.
;; Uses the mark-ring
(use-package! back-button
  :init
  (back-button-mode 1)
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
 :config (setq smog-command "style -L en"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Lexic                                                                            ;;
;;                                                                                  ;;
;; https://github.com/tecosaur/lexic                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! lexic
  :bind
  ("<f8>" . +lookup/dictionary-definition-lexic)
  ("S-<f8>" . lexic-search)
)

(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
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
:bind
   ("C-c s n" . yas-new-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Nov.el                                                                           ;;
;;                                                                                  ;;
;; https://depp.brause.cc/nov.el/                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! nov
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
  (load-theme 'misterioso t)
  )

;; turn on light theme
(defun go-light-theme ()
  (interactive)
  (setq boon-default-cursor-color "black")
  (setq my-theme-shade "light")
  (load-theme 'tsdh-light)
  )

;; turn on poet theme
(defun poet-theme ()
  (interactive)
  (setq boon-default-cursor-color "black")
  (setq my-theme-shade "light")
  (load-theme 'poet t)
  )

;; switch between light and dark theme
(defun switch-theme ()
  (interactive)
  (if (equal my-theme-shade "light")
      (go-dark-theme)
    (go-light-theme)
    )
  )
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
