;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; greek-mode is an emacs minor mode which can be used to
;;; insert greek letters in your files. The shortcut is :
;;; C-, <c>
;;; where <c> is a character from the list below.

;;; ABGDEZHQIKLMNJOPRSTUFXYW
;;; ΑBΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ

;;; abgdezhqiklmnjoprstufxyw
;;; αβγδεζηθικλμnξοπρστυφχψω

(defvar greek-mode-letters-list
  (list '(?A . ?\u0391) ; Alpha
	'(?B . ?\u0392) ; Beta
	'(?G . ?\u0393) ; Gamma
	'(?D . ?\u0394) ; Delta
	'(?E . ?\u0395) ; Epsilon
	'(?Z . ?\u0396) ; Zeta
	'(?H . ?\u0397) ; Eta
	'(?Q . ?\u0398) ; Theta
	'(?I . ?\u0399) ; Iota
	'(?K . ?\u039A) ; Kappa
	'(?L . ?\u039B) ; Lambda
	'(?M . ?\u039C) ; Mu
	'(?N . ?\u039D) ; Nu
	'(?J . ?\u039E) ; Xi
	'(?O . ?\u039F) ; Omicron
	'(?P . ?\u03A0) ; Pi
	'(?R . ?\u03A1) ; Rho
	'(?S . ?\u03A3) ; Sigma
	'(?T . ?\u03A4) ; Tau
	'(?U . ?\u03A5) ; Upsilon
	'(?F . ?\u03A6) ; Phi
	'(?X . ?\u03A7) ; Chi
	'(?Y . ?\u03A8) ; Psi
	'(?W . ?\u03A9) ; Omega

	'(?a . ?\u03B1) ; alpha
	'(?b . ?\u03B2) ; beta
	'(?g . ?\u03B3) ; gamma
	'(?d . ?\u03B4) ; delta
	'(?e . ?\u03B5) ; epsilon
	'(?z . ?\u03B6) ; zeta
	'(?h . ?\u03B7) ; eta
	'(?q . ?\u03B8) ; theta
	'(?i . ?\u03B9) ; iota
	'(?k . ?\u03BA) ; kappa
	'(?l . ?\u03BB) ; lambda
	'(?m . ?\u03BC) ; mu
	'(?n . ?\u03BD) ; nu
	'(?j . ?\u03BE) ; xi
	'(?o . ?\u03BF) ; omicron
	'(?p . ?\u03C0) ; pi
	'(?r . ?\u03C1) ; rho
	'(?s . ?\u03C3) ; sigma (If you need the other sigma symbol, its value is 03C2)
	'(?t . ?\u03C4) ; tau
	'(?u . ?\u03C5) ; upsilon
	'(?f . ?\u03C6) ; phi
	'(?x . ?\u03C7) ; chi
	'(?y . ?\u03C8) ; psi
	'(?w . ?\u03C9) ; omega
	)
  "A list of all greek characters and their unicode value")

(defun greek-mode-add-letter (keymap p)
  "Add letter p (letter . greek-value) to keymap"
  (define-key
    keymap
    (kbd (format "%s%c" "C-, " (car p)))
    ;; The evaluation of p below is quite tricky as p doesn't exists
    ;; when the anonymous function is called. 
    ;; It might exist a simpler solution (using macros ?).
    `(lambda () (interactive) (insert-char (cdr ',p)))) 
  p)

(defvar greek-mode-keymap
  (let ((map (make-sparse-keymap)))
    (mapcar (lambda (p) (interactive) (greek-mode-add-letter map p)) greek-mode-letters-list)
    map)
  "Keymap for greek-mode minor mode")
    
(define-minor-mode greek-mode 
  "Insert greek letters."
  :lighter " greek"
  :keymap greek-mode-keymap
  :global t)

(add-hook 'text-mode-hook 'greek-mode)

(provide 'greek-mode)
