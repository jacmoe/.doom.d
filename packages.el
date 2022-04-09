;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! boon)

(package! ctrlf)

(package! move-text)

(package! flymake-proselint)

(package! mw-thesaurus)

(package! powerthesaurus)

(package! browse-kill-ring)

(package! dimmer)

(package! annotate)

(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "303fcc8d5d85a4ebff2798dab50b2ccc0255ea5f")

(package! org-tracktable)

(package! nov)

(package! hide-mode-line)

(package! poet-theme)

(package! lexic)

(package! typopunct)

(package! smart-quotes)

(package! smog)

(package! back-button)

(package! page-break-lines)

(package! draft-mode)

(package! wwg)

(package! rainbow-mode)

(package! org-super-agenda)

;; (package! orgdiff :recipe (:host github :repo "tecosaur/orgdiff"))

(package! org-transclusion)

(unpin! org-roam)
(package! org-roam-ui)
