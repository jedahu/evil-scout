;;; evil-scout.el -- alternative <leader> implementation for evil

;; Copyright (C) 2013 by Albert Krewinkel

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; Usage:

;; Set <Leader> to "," and <LocalLeader> to "\" (backslash)

;; (setq evil-scout-leader-key ","
;;       evil-scout-local-leader-key "\\")

;; Note that setting both to the same key might lead to conflict (needs
;; fixing).

;; Define "\e" to eval the current top-level sexp the curser is in, but only
;; if emacs-lisp mode is active
;; (define-key-local-leader emacs-lisp-mode-map "e" 'eval-defun)

;; Define ",b" to call ido-switch-buffer.
;; (define-key-leader "b" 'ido-switch-buffer)

;; The previous binding of the leader keys will stay available under
;; <leader><leader> and <localleader><localleader> respectivly.


;;; Code

(require 'evil)

(defgroup evil-scout nil
  "Alternative <Leader> support for evil."
  :group 'evil
  :prefix 'evil-scout-)

(defcustom evil-scout-leader-key ","
  "<Leader> key."
  :type 'string
  :group 'evil-scout)

(defcustom evil-scout-non-normal-leader-key "C-,"
  "<Leader> outside normal mode."
  :type 'string
  :group 'evil-scout)

(defcustom evil-scout-local-leader-key "\\"
  "<LocalLeader> key."
  :type 'string
  :group 'evil-scout)

(defcustom evil-scout-non-normal-local-leader-key nil
  "<LocalLeader> outside normal mode."
  :type 'string
  :group 'evil-scout)

;;; Utilities

(defun evil-scout-build-key-sequence (&rest keys)
  "Build a sequence from individual keys and subsequences.
Accepted arguments types are characters, strings or vectors."
  (cl-flet ((to-key-sequence (key)
              (cond ((characterp key)
                     key)
                    ((stringp key)
                     (read-kbd-macro key))
                    ((sequencep key)
                     (cl-coerce key 'vector)))))
    (let ((result))
      (dolist (key keys (apply #'vconcat (nreverse result)))
        (push (to-key-sequence key) result)))))

;; Functions to get the leader keys as vectors.
(defun evil-scout-leader ()
  (evil-scout-build-key-sequence evil-scout-leader-key))

(defun evil-scout-non-normal-leader ()
  (if (null evil-scout-non-normal-leader-key)
      (vector (cons 'control (coerce (evil-scout-leader) 'list)))
    (evil-scout-build-key-sequence
     evil-scout-non-normal-leader-key)))

(defun evil-scout-local-leader ()
  (evil-scout-build-key-sequence evil-scout-local-leader-key))

(defun evil-scout-non-normal-local-leader ()
  (if (null evil-scout-non-normal-local-leader-key)
      (vector (cons 'control (coerce (evil-scout-local-leader) 'list)))
    (evil-scout-build-key-sequence
     evil-scout-non-normal-local-leader-key)))

;;; Leader Key

(defun evil-scout-leader-keys (c)
  "Create a complete key sequence by prepending the leader key."
  (evil-scout-build-key-sequence evil-scout-leader-key c))

(defun evil-scout-non-normal-leader-keys (c)
  "Create a complete key sequence by prepending the non-normal leader key."
  (evil-scout-build-key-sequence (evil-scout-non-normal-leader) c))

(defun evil-scout-local-leader-keys (c)
  "Create a complete key sequence for the local leader."
  (evil-scout-build-key-sequence (evil-scout-local-leader)
                                 c))

(defun evil-scout-non-normal-local-leader-keys (c)
  (evil-scout-build-key-sequence (evil-scout-non-normal-local-leader)
                                 c))

(defmacro evil-scout-redefine-local-leader (state keymap leader)
  `(let ((state ,state)
         (keymap ,keymap)
         (leader ,leader))
     ;; FIXME: This assumes that the current binding of leader is a simple
     ;; command.  If the leader key is already bound to a keymap, it is
     ;; assumed that this is all right, no rebinding will occur.
     (let ((old-binding (lookup-key keymap leader)))
      (unless (keymapp old-binding)
        ;; unbind leader key in this keymap
        (evil-define-key state keymap leader nil)
        ;; Redefine leader key as prefix, make old behavior available as
        ;; <leader><leader>
        (evil-define-key state keymap
          (evil-scout-build-key-sequence leader leader)
          old-binding)))))

(defmacro evil-scout--define-leader (keymap leader key def)
  `(progn
     (evil-define-key 'normal ,keymap ,leader ,key ,def)))

(defmacro define-key-local-leader (keymap key def)
  `(let* ((keymap ,keymap)
          (key    ,key)
          (def    ,def))
      (progn
       (evil-scout-redefine-local-leader 'insert keymap
                                   (evil-scout-non-normal-local-leader))
       (evil-define-key 'insert keymap
         (evil-scout-non-normal-local-leader-keys key) def)

       (evil-scout-redefine-local-leader 'normal keymap
                                   (evil-scout-local-leader))
       (evil-define-key 'normal keymap
         (evil-scout-local-leader-keys key) def))))

(defmacro define-key-leader (key def)
  `(let ((key ,key)
         (def ,def))
     ;; FIXME: combine the pairwise corresponding sexps into single function
     ;; or macro calls.
     (evil-scout-redefine-local-leader nil evil-insert-state-map
       (evil-scout-non-normal-leader))
     (define-key evil-insert-state-map
       (evil-scout-non-normal-leader-keys key) def)

     (evil-scout-redefine-local-leader nil evil-visual-state-map
       (evil-scout-non-normal-leader))
     (define-key evil-visual-state-map
       (evil-scout-non-normal-leader-keys key) def)

     (evil-scout-redefine-local-leader nil evil-normal-state-map
       (evil-scout-leader))
     (define-key evil-normal-state-map
       (evil-scout-leader-keys key) def)))

(provide 'evil-scout)
;;; evil-scout.el ends here
