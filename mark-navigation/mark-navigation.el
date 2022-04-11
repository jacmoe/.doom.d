;;; mark-navigation.el --- mark navigation commands   -*- lexical-binding: t -*-
;;
;; -*- coding: utf-8 -*-
;;
;; Copyright © 2016 Andrew L. Moore

;; Author: Andrew L. Moore <alm@gnu.org>
;; Keywords: editing, languages, lisp
;; URL: https://github.com/slewsys/emacs-extensions

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; `mark-navigation-mode' is a minor mode that provides bindings for
;; forward and backward traversal of local and global mark rings.

;;; Installation:
;;
;;   (require 'mark-navigation)
;;
;; To enable `mark-navigation-mode' globally:
;;
;;   (mark-navigation-mode)
;;
;; To enable locally - per major mode only, .e.g, `elisp-mode' - use instead:
;;
;;   (add-hook 'elisp-mode-hook #'mark-navigation-local-mode)
;;
;; To enable globally with some major mode exceptions, e.g., `elisp-mode', use:
;;
;;   (mark-navigation-mode)
;;   (add-hook 'elisp-mode-hook
;;             (lambda ()
;;               (mark-navigation-local-mode -1)))
;;
(defgroup mark-navigation ()
  "Navigate mark ring."
  :package-version '(mark-navigation . "1.1")
  :group 'convenience)

(defcustom mark-navigation-bindings
  `(
    ;;; Previous mark    ;;; Next mark
    ,(kbd "C-c M-p")    ,(kbd "C-c M-n") ;;; C-c bindings.
    ,(kbd "M-p")        ,(kbd "M-n")     ;;; Meta-bindings.
    )
  "List key-sequences bound to functions
`mark-navigation-next-mark' and `mark-navigation-previous-mark'."
  :tag "Mark navigation bindings"
  :version "25.1"
  :group 'mark-navigation
  :type '(repeat key-sequence))

(defvar mark-navigation-ring-order 'backward
  "The order in which marks in the current buffer's mark-ring are visited,
   either `forward' or `backward', the default.")
(make-variable-buffer-local 'mark-ring-traversal)
(put 'mark-ring-traversal 'permanent-local t)

(defvar mark-navigation-global-ring-order 'backward
  "The order in which marks in the global-mark-ring are visited,
   either `forward' or `backward', the default.")

(defun init-mark-navigation-map (bindings-sym)
  "Initialize keymap for `mark-navigation-mode'."
  (let ((map (make-sparse-keymap))
        (i 0))
    (mapc
     (lambda (binding)
       (if (= (setq i (lognot i)) 0)
           (define-key map binding 'mark-navigation-next-mark)
         (define-key map binding 'mark-navigation-previous-mark)))
     (symbol-value bindings-sym))
    map))

(defmacro mark-navigation-set-mark (ring ring-order direction kth)
  "Visit `kth' mark in `ring' in `ring-order', which is set to `direction'."
  `(progn
     (setq this-command 'set-mark-command)
     (when (not (equal ,ring-order ,direction))
       (setq ,ring-order ,direction)
       (setq ,ring (reverse ,ring))
       (setq ,kth (+ ,kth 2)))
     (let ((mark-ring-p (eq ,ring mark-ring))
           (set-mark-command-repeat-pop t))
       (dotimes (i ,kth)
         (set-mark-command mark-ring-p)))))

(defun mark-navigation-pop-mark (direction kth)
  "Jump to `kth' next or previous mark, depending on whether `direction'
   is `forward' or `backward', respectively."
  (if (eq last-command 'pop-global-mark)
      (mark-navigation-set-mark global-mark-ring
                   mark-navigation-global-ring-order direction kth)
    (mark-navigation-set-mark mark-ring
                              mark-navigation-ring-order direction kth)))

(defun mark-navigation-pop-again-mark (kth)
  "Go to current mark, push it onto mark-ring and pop previous
   mark. With prefix argument, `kth', go to kth previous mark. For
   full description, see `set-mark-command' command."
  (interactive "p")
  (mark-navigation-pop-mark mark-navigation-ring-order kth))

(defun mark-navigation-previous-mark (kth)
  "Go to current mark, push it onto mark-ring and pop previous
   mark. With prefix argument, `kth', go to kth previous mark. For
   full description, see `set-mark-command' command."
  (interactive "p")
  (mark-navigation-pop-mark 'backward kth))

(defun mark-navigation-next-mark (kth)
  "Go to current mark, push it onto mark-ring and pop next mark.
   With prefix argument, `kth', go to kth next mark. For full
   description, see `set-mark-command' command."
  (interactive "p")
  (mark-navigation-pop-mark 'forward kth))

;;; FIXME: Prevent mark duplication.
(defun mark-navigation-clear-mark (&optional global)
  "Clear mark from local mark ring. With prefix argument, clear
mark from global mark ring."
  (interactive "P")
  ;; (if global (setq global-mark-ring (cdr global-mark-ring))
  ;;   (setq global-mark-ring (cdr global-mark-ring)))
  ;; (push-mark-command nil t)
  (when mark-active (force-mode-line-update))
  (set-marker (mark-marker) nil (current-buffer))
  (setq mark-active nil))

(defun mark-navigation-clear-marks (&optional global)
  "Deactivate mark and clear local mark ring.
   With prefix argument, clear global mark ring."
  (interactive "P")
  (if global (setq global-mark-ring nil)
    (progn
      (setq mark-ring nil)
      (when mark-active (force-mode-line-update))
      (set-marker (mark-marker) nil (current-buffer))
      (setq mark-active nil))))


;;;###autoload
(define-minor-mode mark-navigation-mode
  "Toggles variable `mark-navigation-mode'.

 Interactively with no argument, this command toggles the mode.
With a positive prefix argument, enables the mode. With any other
prefix argument, disables it.

From Lisp (i.e., non-interactively), argument omitted or nil
enables the mode, `toggle' toggles the state.

`mark-navigation-mode' is a minor mode that provides bindings for
traversing the local and global mark rings."

  ;;; No initial value.
  :init-value nil

  ;;; No mode line indicator.
  :lighter nil

  ;;; Minor mode keymap
  :keymap (init-mark-navigation-map 'mark-navigation-bindings)

  ;;; Gobal variable `mark-navigation-mode'.
  :global t

  ;;; Customization group
  :group 'mark-navigation

  ;;; Disable `mark-navigation-mode' in minibuffer and comint-mode.
  (when mark-navigation-mode
    (add-to-list 'minibuffer-setup-hook
                 (lambda ()
                   (mark-navigation-local-mode -1)))
    (add-hook 'comint-mode-hook
                 (lambda ()
                   (mark-navigation-local-mode -1)))))

;;;###autoload
(define-minor-mode mark-navigation-local-mode
  "Toggle `mark-navigation-mode' only in this buffer."
  :variable (buffer-local-value 'mark-navigation-mode (current-buffer))
  (cond
   ((eq mark-navigation-mode (default-value 'mark-navigation-mode))
    (kill-local-variable 'mark-navigation-mode))
   ((not (default-value 'mark-navigation-mode))

    ;;; Enable locally.
    (mark-navigation-mode)

    ;;; Disable globally elsewhere.
    (setq-default mark-navigation-mode nil))))

(provide 'mark-navigation)

;;; mark-navigation.el ends here
