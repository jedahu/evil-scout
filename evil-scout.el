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

;;; WRITEME

;;; Code

(require 'evil)

(defgroup evil-scout nil
  "Alternative <Leader> support for evil."
  :group 'evil
  :prefix 'evil-scout-)

(defcustom evil-scout-keys-alist
  `((leader "," ,evil-normal-state-map ,evil-visual-state-map)
    (local-leader "\\" normal visual insert))
  "Assoc list of <leader> types, assigned keys, and states."
  :type 'list
  :group 'evil-scout)

(defcustom evil-scout-keys-in-other-states '(leader local-leader)
  "List of <leader>s which should be available in non-listed states."
  :type 'list
  :group 'evil-scout)

(defcustom evil-scout-global-leaders '(leader)
  "List of global <leader>s."
  :type 'list
  :group 'evil-scout)

(defcustom evil-scout-other-states-modifier 'control
  "Modifier key to use <leader> in non-listed states."
  :type 'symbol
  :group 'evil-scout)

;;; Utilities

(defun evil-scout-leader-key (leader)
  (second (assoc leader evil-scout-keys-alist)))

(defun evil-scout-leader-states (leader)
  (cddr (assoc leader evil-scout-keys-alist)))

(defun evil-scout-build-key-sequence (&rest keys)
  "Build a sequence from individual keys and subsequences.
Accepted arguments types are characters, strings or vectors.

Example:
  (evil-scout-build-key-sequence [(control ?e)] \"vi\" ?l)
  =>  [(control 101) 118 105 108] ; i.e. \"\C-e vil\""
  (cl-flet ((to-key-sequence (key)
              (cond ((characterp key)
                     (list key))
                    ((stringp key)
                     (read-kbd-macro key t))
                    ((sequencep key)
                     (cl-coerce key 'vector)))))
    (let ((result))
      (dolist (key keys (apply #'vconcat (nreverse result)))
        (push (to-key-sequence key) result)))))

;; Access the configs
(defun evil-scout-key-sequence (leader &optional keys)
  "Get key sequence for `leader'."
  (evil-scout-build-key-sequence
   (evil-scout-leader-key leader)
   keys))

(defun evil-scout-in-other-states-p (leader)
  "If `leader' should be accessible in non-normal state."
  (find leader evil-scout-keys-in-other-states))

(defun evil-scout-other-states-key-sequence (leader &optional keys)
  "The key sequence for `leader' in non-normal state."
  (when (evil-scout-in-other-states-p leader)
    (evil-scout-build-key-sequence
     (vector (cons evil-scout-other-states-modifier
                   (coerce (evil-scout-key-sequence leader) 'list)))
     keys)))

(defvar evil-scout-local-states
  '(emacs insert motion visual normal))

(defvar evil-scout-global-states
  (list evil-emacs-state-map
        evil-insert-state-map
        evil-motion-state-map
        evil-visual-state-map
        evil-normal-state-map))

;;; Leader Key
(defun define-leader-key (leader keymap key def)
  (let ((leader leader)
        (keymap keymap)
        (key    key)
        (def    def))
    (let ((other-seq     (evil-scout-other-states-key-sequence leader key))
          (leader-seq    (evil-scout-key-sequence leader key))
          (leader-states (evil-scout-leader-states leader)))
      (mapc #'(lambda (state)
                (evil-define-key state keymap leader-seq def))
            leader-states)
      (mapc #'(lambda (state)
                (unless (member state leader-states)
                  (evil-define-key state keymap other-seq def)))
            evil-scout-local-states))))

(defun define-global-leader (key def)
  (let ((key    key)
        (def   def))
    (let ((other-seq   (evil-scout-other-states-key-sequence 'leader key))
          (leader-seq  (evil-scout-key-sequence              'leader key))
          (leader-maps (evil-scout-leader-states 'leader)))
      (mapc #'(lambda (state-map)
                (define-key state-map leader-seq def))
            leader-maps)
      (mapc #'(lambda (state-map)
                (unless (member state-map leader-maps)
                  (define-key state-map other-seq def)))
	    evil-scout-global-states))))

(defun evil-scout-reset (leader keymap)
  (define-leader-key leader keymap nil nil))

(provide 'evil-scout)
;;; evil-scout.el ends here
