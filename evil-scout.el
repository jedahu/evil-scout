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
      (vector (list 'control evil-scout-leader-key))
    (evil-scout-build-key-sequence
     evil-scout-non-normal-leader-key)))

(defun evil-scout-local-leader ()
  (evil-scout-build-key-sequence evil-scout-local-leader-key))

(defun evil-scout-non-normal-local-leader ()
  (if (null evil-scout-non-normal-local-leader-key)
      (vector (list 'control evil-scout-local-leader-key))
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

(defmacro define-key-local-leader (keymap key def)
  `(let ((keymap ,keymap)
         (key    ,key)
         (def    ,def))
     (evil-define-key 'insert keymap
       (evil-scout-non-normal-local-leader-keys key) def)
     (evil-define-key 'normal keymap
       (evil-scout-local-leader-keys key) def)))

(defmacro define-key-leader (key def)
  `(let ((key ,key)
         (def ,def))
     (define-key evil-insert-state-map
       (evil-scout-non-normal-leader-keys key) def)
     (define-key evil-visual-state-map
       (evil-scout-non-normal-leader-keys key) def)
     (define-key evil-normal-state-map
       (evil-scout-leader-keys key) def)))

(provide 'evil-scout)
;;; evil-scout.el ends here
