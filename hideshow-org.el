;;; hideshow-org.el - Provides org-mode like hide and show for hideshow.el
;;
;; Copyright (C) 2009 Shane Celis
;;
;; Author: Shane Celis <shane (at) gnufoo (dot) org>
;; Keywords: C C++ java lisp tools editing comments blocks hiding outlines org-mode
;; This file is not part of GNU Emacs.
;;
;; hideshow-org.el free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; hideshow-org.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; hideshow-org.el provides an org-mode like interface to the
;; hideshow.el file.
;;
;; org-mode provides an elegant means of interacting with outlines
;; that one can toggle with the TAB key and Shift TAB.
;; hideshow-org.el is my attempt to bring the org-mode like hiding and
;; showing to code.
;;
;;; Download:
;;
;; $ git clone git://github.com/secelis/hideshow-org.git
;;
;;; Installation:
;;
;; (add-to-list 'load-path "/path/to/hideshow-org-directory")
;; (require 'hideshow-org)
;; 
;;; Keymaps:
;;
;; I set this as my global key.
;;
;; (global-set-key "\C-ch" 'hs-org/minor-mode)
;;
;; Here are the keys in the minor mode.
;;
;; TAB       -- execute normal TAB command, if point doesn't move, try to
;;              toggle the visibility of the block.
;; <S-tab>   -- execute normal <S-tab command, if point doesn't move, try to
;;              toggle the visibility of all the blocks.
;;
;;; Notes:
;;
;; The hardest part was trying to figure out when TAB should behave
;; normally: for code that usually means some variant of *-ident-line
;; (and is incidentally one of the reason I love emacs); for outlines
;; in org-mode that means toggling the visibility.  So the solution I
;; came up with is this.  If TAB doesn't change the point, then we try
;; to hide or show the block.  And we do a similar thing for Shift
;; TAB.  Hopefully, this will be sufficient such that
;; hs-org/minor-mode does not get in the way of anyone's normal
;; programming habits.
;; 
;; Many thanks to the developers of hideshow.el.  Thanks to
;; yasnippets.el for showing me how one could piggyback on an already
;; bound key.

(defvar hs-org/trigger-key-block (kbd "TAB")
  "The key to bind to toggle block visibility.")

(defvar hs-org/trigger-key-all (kbd "<S-tab>")
  "The key to bind to toggle all block visibility.")

(defvar hs-org/minor-mode-map nil
  "The keymap of hs-org/minor-mode")

(unless hs-org/minor-mode-map
  (setq hs-org/minor-mode-map (make-sparse-keymap)))

(defvar hs-org/hide-show-all-next nil
  "Keeps the state of how the buffer was last toggled by Shift TABing.")

(make-variable-buffer-local 'hs-org/hide-show-all-next)

(define-minor-mode hs-org/minor-mode
  "Toggle hs-org minor mode.
With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When hs-org minor mode is enabled, the TAB key toggles the
visible state of the code, and shift TAB toggles the visible
state of the entire file.

You can customize the key through `hs-org/trigger-key-block'."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.  Nothing.  hs will already be in there.
  ""
  :group 'editing
  (define-key hs-org/minor-mode-map hs-org/trigger-key-block 'hs-org/hideshow)
  (define-key hs-org/minor-mode-map hs-org/trigger-key-all 'hs-org/hideshow-all)
  ;; We want hs-minor-mode on when hs-org/minor-mode is on.
  (when (and hs-org/minor-mode (not hs-minor-mode))
      (hs-minor-mode t))
  (let ((hs (cdr (assoc 'hs-minor-mode minor-mode-alist))))
    (if hs-org/minor-mode
        (setcar hs (concat (car hs) "+"))
        (setcar hs (replace-regexp-in-string "\\++$" "" (car hs)))
        )))

(defun hs-org/hideshow ()
  "Hide or show a block."
  (interactive)
  (let* ((last-point (point))
         (hs-org/minor-mode nil)
         (command (key-binding hs-org/trigger-key-block)))
    (when (commandp command)
      (call-interactively command))
    (when (equal last-point (point))
      (hs-toggle-hiding))))

(defun hs-org/hideshow-all ()
  "Hide or show all blocks."
  (interactive)
  (let* ((last-point (point))
         (hs-org/minor-mode nil)
         (command (key-binding hs-org/trigger-key-all)))
    (when (commandp command)
      (call-interactively command))
    (when (equal last-point (point))
      (if hs-org/hide-show-all-next
          (hs-show-all)
          (hs-hide-all))
      (setq hs-org/hide-show-all-next (not hs-org/hide-show-all-next)))))
  
(provide 'hideshow-org)
