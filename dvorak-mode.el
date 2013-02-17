;;; dvorak-mode.el --- Minor mode for convert qwerty input to dvorak input

;;  Copyright (C) 2013  traveler

;;; Author: traveler
;;; URL: https://github.com/traveter/dvorak-mode

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Usage

;; Put this file in your Emacs lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'dvorak-mode)
;; ex (global-set-key (kbd "C-^") 'dvorak-mode)

(defvar dvorak:qwerty-key-str "asdfghjkl;:qwertyuiopzxcvbnm,./ASDFGHJKL+*QWERTYUIOPZXCVBNM<>?")
(defvar dvorak:dvorak-key-str "aoeuidhtns-',.pyfgcrl;qjkxbmwvzAOEUIDHTNS_\"<>PYFGCRL:QJKXBMWVZ")

(defconst dvorak:mode-indicator " Dvorak")
(defvar dvorak:dvorak-mode nil)
(defvar dvorak:dvorak-mode-map nil)
(defvar dvorak:qwerty-to-dvorak-hash)

(defun dvorak-mode-off ()
  "dvorak-mode off"
  (setq dvorak:dvorak-mode nil
	minor-mode-map-alist (delete (assq 'dvorak:dvorak-mode minor-mode-map-alist) minor-mode-map-alist)
	minor-mode-alist (delete (assq 'dvorak:dvorak-mode minor-mode-alist) minor-mode-alist)))

(defun dvorak-mode-on ()
  "dvorak-mode on"
  (let ((escape nil)
	(qwerty-char)
	(dvorak-str)
	(qwerty-key-list)
	(dvorak-key-list))
    (setq dvorak:qwerty-to-dvorak-hash (make-hash-table :test #'equal))
    ;; (dotimes (i (length dvorak:qwerty-key-str))
    ;;   (if escape
    ;; 	  (setq qwerty-char (substring dvorak:qwerty-key-str (1- i) (1+ i))
    ;; 		escape nil)
    ;; 	(setq qwerty-char (substring dvorak:qwerty-key-str i (1+ i))
    ;; 	      dvorak-char (substring dvorak:dvorak-key-str i (1+ i)))
    ;; 	)
    ;;   (cond ((string= qwerty-char "") nil)
    ;; 	    ((string= qwerty-char "\\") (setq escape t))
    ;; 	    ((string= dvorak-char "") nil)
    ;; 	    (t (add-to-list 'qwerty-key-list qwerty-char)
    ;; 	       (add-to-list 'dvorak-key-list dvorak-char)
    ;; 	       )
    ;; 	    )
    ;;   )
    (setq qwerty-key-list (split-string dvorak:qwerty-key-str "" "")
	  dvorak-key-list (split-string dvorak:dvorak-key-str "" ""))
    (dotimes (i (length qwerty-key-list))
      (puthash (nth i qwerty-key-list) (nth i dvorak-key-list) dvorak:qwerty-to-dvorak-hash)
      )
    (setq dvorak:dvorak-mode-map (make-sparse-keymap))
    (dotimes (i (length qwerty-key-list))
      (define-key dvorak:dvorak-mode-map (nth i qwerty-key-list) 'dvorak:insert-dvorak-key))
    (setq dvorak:dvorak-mode t
	  minor-mode-map-alist (cons (cons 'dvorak:dvorak-mode dvorak:dvorak-mode-map) minor-mode-map-alist))
    (add-to-list 'minor-mode-alist '(dvorak:dvorak-mode dvorak:mode-indicator))
    ))

(defun dvorak:insert-dvorak-key ()
  (interactive)
  (insert (gethash (char-to-string last-command-event) dvorak:qwerty-to-dvorak-hash))
  )

(defun dvorak-mode () "dvorak-mode"
  (interactive)
  (if (not dvorak:dvorak-mode)
      (dvorak-mode-on)
    (dvorak-mode-off)))

(provide 'dvorak-mode)
;;; dvorak-mode.el ends here
