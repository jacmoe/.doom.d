;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! boon)

(package! ctrlf)

(package! org-tracktable)

(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "303fcc8d5d85a4ebff2798dab50b2ccc0255ea5f")

(package! org-super-agenda)

(package! org-transclusion)

(package! annotate)

(package! typopunct)

(package! smart-quotes)

(package! flymake-proselint)

(package! smog)

;; (package! orgdiff :recipe (:host github :repo "tecosaur/orgdiff"))

(package! mw-thesaurus)

(package! powerthesaurus)

(package! browse-kill-ring)

(package! dimmer)

(package! nov)

(package! hide-mode-line)

(package! poet-theme)

(package! lexic)

(package! page-break-lines)

(package! draft-mode)

(package! wwg)

(package! rainbow-mode)

(unpin! org-roam)
(package! org-roam-ui)

(package! mark-navigation
  :recipe (:local-repo "mark-navigation"))

(package! citeproc)
