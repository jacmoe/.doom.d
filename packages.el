;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! boon)

(package! ctrlf)

(package! org-tracktable)

(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "303fcc8d5d85a4ebff2798dab50b2ccc0255ea5f")

(package! annotate)

(package! typopunct)

(package! smart-quotes)

(package! flymake-proselint)

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

(package! ef-themes)

(package! anki-editor)

(package! org-wc)

(package! writegood-mode :recipe (:host github :repo "bnbeckwith/writegood-mode"))
