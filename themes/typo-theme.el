;;; typo-theme.el --- Typographic (not color) Theme

;; Copyright (C) 2017 Bastian Bechtold

;; Author: Bastian Bechtold
;; URL: https://github.com/bastibe/.emacs.d/tree/master/lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A color theme without color. Like all text documents *except* source
;; code, this theme uses typography to distinguish between different
;; parts of text.
;;
;; Color-based highlighting is an anachronism borne from terminals'
;; inability to switch fonts. All we had was colors, so colors is what
;; we used. But in todays graphical world, this is no longer necessary,
;; and Emacs can use any font we like.
;;
;; I like PragmataPro. This theme is based on PragmataPro, and adds a
;; few other fonts for good measure. Strings are Iosevka Slab. Comments
;; are oblique Iosevka. Documentation is regular Iosevka. Headlines are
;; InputSerifCompressed. Ubuntu Mono works well, too.

;;; Credits:

;; Forked From:
;;
;; eink-emacs - Eink color theme for Emacs
;; Marian Schubert <marian.schubert@gmail.com>
;; http://github.com/maio/eink-emacs

;; Inspired by:
;;
;; https://bitbucket.org/kisom/eink.vim
;; https://github.com/dmand/eink.el
;; http://www.daveliepmann.stfi.re/tufte-css/?sf=wklwy

;;; Code:

(deftheme typo
  "Theme emulating reading on an E Ink device.")

(let* ((fg "#111111")
       (bg "#fffff8")
       (bg-light "#ddddd8")
       (fg-medium "#404040")
       (fg-light "#606060")
       (bg-lighter "#f4f4f0")
       (bg-white "#fcfcf8")
       (bg-highlight "#FFF1AA")
       (bg-highlight-2 "LightCyan")
       (bg-highlight-3 "LightGreen")
       (headline-1 `(:foreground ,fg :weight semi-bold :height 1.4 :family "InputSerifCompressed"))
       (headline-2 `(:foreground ,fg :weight semi-bold :height 1.4 :family "InputSerifCompressed"))
       (headline-3 `(:foreground ,fg :weight semi-bold :height 1.2 :family "Iosevka Slab"))
       (headline-4 `(:foreground ,fg :weight semi-bold :height 1.1)))


  (custom-theme-set-faces
   'typo

   ;; generic stuff
   `(default ((t (:background ,bg :foreground ,fg :family "PragmataPro"))))
   `(button ((t (:foreground ,fg :underline t))))
   `(cursor ((t (:background ,fg :foreground "white smoke"))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t))))
   `(font-lock-builtin-face ((t (:foreground ,fg-medium)))) ; nicht sichtbar
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg :slant oblique :weight light :family "Iosevka"))))
   `(font-lock-comment-face ((t (:foreground ,fg :slant oblique :weight light :family "Iosevka"))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-doc-face ((t (:foreground ,fg :weight light :family "Iosevka"))))
   `(font-lock-function-name-face ((t (:foreground ,fg :underline t))))
   `(font-lock-keyword-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,fg-light :family "Iosevka Slab" :weight light)))) ; nicht sichtbar
   `(font-lock-type-face ((t (:foreground ,fg :underline t))))
   `(font-lock-variable-name-face ((t (:foreground ,fg-medium :underline nil)))) ; nicht sichtbar
   `(font-lock-warning-face ((t (:foreground ,fg :weight bold))))
   `(fringe ((t (:background ,bg :foreground ,fg))))
   `(gnus-header-content ((t (:foreground ,fg))))
   `(gnus-header-from ((t (:foreground ,fg))))
   `(gnus-header-name ((t (:foreground ,fg))))
   `(gnus-header-subject ((t (:foreground ,fg))))
   `(highlight ((t nil)))
   `(ido-first-match ((t (:foreground ,fg :weight bold))))
   `(ido-vertical-first-match ((t (:foreground ,fg :weight bold))))
   `(ido-only-match ((t (:foreground ,fg))))
   `(ido-subdir ((t (:foreground ,fg))))
   `(isearch ((t (:foreground ,fg :box (:line-width -1)))))
   `(isearch-lazy-highlight-face ((t (:foreground ,fg :box (:line-width -1)))))
   `(link ((t (:foreground ,fg))))
   `(minibuffer-prompt ((t (:foreground ,fg-medium :weight bold))))
   `(mode-line ((t (:background ,bg-light :foreground ,fg :height 1.0))))
   `(mode-line-buffer ((t (:foreground ,fg :weight bold))))
   `(mode-line-inactive ((t (:background ,bg-lighter :foreground ,fg-light :height 1.0))))
   `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(modeline ((t (:background ,bg :foreground ,fg :height 1.0))))

   ;; latex
   `(font-latex-bold-face ((t (:foreground ,fg))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground "#a9a9a9"))))
   `(font-latex-sectioning-5-face ((t (:foreground ,fg :weight bold))))
   `(font-latex-math-face ((t (:foreground ,fg))))
   `(font-latex-warning-face ((t (:foreground ,fg :weight bold))))
   `(font-latex-sedate-face ((t (:foreground ,fg :weight bold))))
   `(font-latex-sectioning-1-face ((t ,headline-1)))
   `(font-latex-sectioning-2-face ((t ,headline-2)))
   `(font-latex-sectioning-3-face ((t ,headline-3)))
   `(font-latex-sectioning-4-face ((t ,headline-4)))
   `(font-latex-sectioning-5-face ((t ,headline-4)))

   ;; org
   `(org-agenda-date ((t (:foreground ,fg :height 1.2))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold :height 1.4))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:background ,bg-white :foreground ,fg))))
   `(org-block-background ((t (:background ,bg-white))))
   `(org-block-begin-line ((t (:foreground ,fg, :background ,bg-lighter :family "Iosevka Slab"))))
   `(org-block-end-line ((t (:foreground ,fg :background ,bg-lighter :family "Iosevka Slab"))))
   `(org-meta-line ((t (:foreground ,fg :background ,bg-lighter :family "Iosevka Slab"))))
   `(org-code ((t (:foreground ,fg-medium :background ,bg-white :family "Iosevka Slab"))))
   `(org-date ((t (:foreground ,fg) :underline)))
   `(org-hide ((t (:foreground ,bg))))
   `(org-document-title ((t ,headline-1)))
   `(org-document-info ((t (:foreground ,fg))))
   `(org-document-info-keyword ((t (:foreground ,fg-light :family "Iosevka Slab"))))
   `(org-level-1 ((t ,headline-2)))
   `(org-level-2 ((t ,headline-3)))
   `(org-level-3 ((t ,headline-4)))
   `(org-level-4 ((t ,headline-4)))
   `(org-level-5 ((t ,headline-4)))
   `(org-level-6 ((t ,headline-4)))
   `(org-link ((t (:foreground ,fg :underline t))))
   `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-sexp-date ((t (:foreground ,fg))))
   `(org-special-keyword ((t (:foreground ,fg))))
   `(org-todo ((t (:foreground ,fg :family "Iosevka Slab"))))
   `(org-done ((t (:foreground ,fg-light :family "Iosevka Slab"))))
   `(org-verse ((t (:inherit org-block :slant italic))))
   `(org-table ((t (:foreground ,fg))))

   `(region ((t (:background "#eeeee8" :foreground ,fg))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(whitespace-line ((t (:background ,bg-highlight-2))))
   `(whitespace-space ((t (:background ,bg :family "Iosevka"))))
   `(whitespace-newline ((t (:background ,bg :family "Iosevka"))))
   `(whitespace-empty ((t (:background ,bg :family "Iosevka"))))
   `(whitespace-trailing ((t (:background ,bg-highlight-2))))

   ;; magit
   `(magit-section-heading ((t (:weight bold :height 1.2))))
   `(magit-branch-local ((t (:weight bold))))
   `(magit-branch-remote ((t (:weight bold))))
   `(magit-branch-current ((t (:weight bold :box (:line-width -1)))))

   ;; markdown
   `(markdown-header-face-1 ((t ,headline-2)))
   `(markdown-header-face-2 ((t ,headline-3)))
   `(markdown-header-face-3 ((t ,headline-4)))
   `(markdown-header-face-4 ((t ,headline-4)))
   `(markdown-header-face-5 ((t ,headline-4)))
   `(markdown-header-face-6 ((t ,headline-4)))
   `(markdown-pre-face ((t (:foreground ,fg-medium :family "Iosevka Slab"))))
   `(markdown-inline-code-face ((t (:foreground ,fg-medium :family "Iosevka Slab"))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:weight bold))))
   `(dired-subtree-depth-1-face ((t (:background "grey90"))))

   ;; helm
   `(helm-source-header ((t (:foreground ,fg :background "grey90" :weight bold))))
   `(helm-header ((t (:foreground ,fg))))
   `(helm-selection-line ((t (:inherit region :weight bold))))
   `(helm-selection ((t (:background ,bg-highlight))))
   `(helm-ff-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,fg :slant italic))))
   `(helm-ff-executable ((t (:foreground ,fg))))

   ;; iedit
   `(iedit-occurrence ((t (:background ,bg-highlight-3 :foreground ,fg))))

   ;; company
   `(company-echo-common ((t (:foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,bg-highlight))))

   ;; parens - parenface
   '(parenface-paren-face ((t (:foreground "gray70"))))
   '(parenface-curly-face ((t (:foreground "gray70"))))
   '(parenface-bracket-face ((t (:foreground "gray70"))))

   ;; parens - paren-face
   '(parenthesis ((t (:foreground "gray70"))))

   ;; parens - other
   `(sp-show-pair-match-face ((t (:foreground "black" :weight bold :underline t))))
   `(sp-show-pair-mismatch-face ((t (:background "red" :foreground "black" :weight bold))))
   `(show-paren-match ((t (:foreground "black" :weight bold :underline t))))
   `(show-paren-mismatch ((t (:background "red" :foreground "black" :weight bold))))

   ;; js2
   `(js2-function-param ((t (:foreground ,fg))))
   `(js2-external-variable ((t (:foreground ,fg))))

   ;; perl
   `(cperl-hash-face ((t (:foreground ,fg))))
   `(cperl-array-face ((t (:foreground ,fg))))
   `(cperl-nonoverridable-face ((t (:foreground ,fg))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:inherit default))))
   `(rpm-spec-package-face ((t (:inherit default))))
   `(rpm-spec-macro-face ((t (:inherit default))))
   `(rpm-spec-doc-face ((t (:inherit default))))
   `(rpm-spec-var-face ((t (:inherit default))))
   `(rpm-spec-ghost-face ((t (:inherit default))))
   `(rpm-spec-section-face ((t (:inherit default :weight bold))))

   ;; misc
   `(idle-highlight ((t (:background ,bg-highlight))))
   `(yas-field-highlight-face ((t (:background "#eeeee8" :foreground ,fg))))
   `(eshell-prompt ((t (:foreground ,fg :weight bold))))
   `(cider-result-overlay-face ((t (:weight bold))))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'typo)
;;; typo-theme.el ends here
