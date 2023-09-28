;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Fix for org-roam link issue
(package! org :pin "ca873f7")

(package! boon)

(package! ctrlf)

(package! org-tracktable)

(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "303fcc8d5d85a4ebff2798dab50b2ccc0255ea5f")

(package! annotate)

(package! typopunct)

(package! smart-quotes)

(package! mw-thesaurus)

(package! powerthesaurus)

(package! browse-kill-ring)

(package! dimmer)

(package! nov)

(unpin! org-roam)
(package! org-roam-ui)

(package! citeproc) ; for Ox-Hugo when it exports citations in CSL format

(package! atomic-chrome)

(package! engine-mode)

(package! anki-editor)

(package! org-wc)

(package! grab-x-link) ; use grab-mac-link if you are a Mac user

(package! speed-type)

(package! org-analyzer)

(package! mastodon)

(package! flymake-vale :recipe (:host github :repo "tpeacock19/flymake-vale"))

(package! define-word)

(package! mermaid-mode)

(package! org-transclusion)

(package! org-super-agenda)

(package! keycast)

(package! gif-screencast)

(package! olivetti)

;; (package! org-habit-stats)

(package! poet-theme)

(package! mixed-pitch)

(package! elpher)

(package! gemini-write)

(package! org-heatmap :recipe (:host github :repo "Elilif/org-heatmap"))

(package! sandcastle-theme :recipe (:host github :repo "habamax/sandcastle-theme"))
