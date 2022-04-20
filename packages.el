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

;; (package! lexic :recipe (:host github :repo "tecosaur/lexic"))
(package! lexic :recipe (:local-repo "lexic"))

(package! draft-mode)

(package! wwg)

(package! rainbow-mode)

(unpin! org-roam)
(package! org-roam-ui)

(package! citeproc)

(package! stimmung-themes)

(package! uwu-theme
   :recipe (:host github :repo "kborling/uwu-theme"))

(package! catppuccin-theme)

(package! org-super-links
   :recipe (:host github :repo "toshism/org-super-links"))

(package! palimpsest)
