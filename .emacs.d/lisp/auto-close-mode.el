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


;;; auto-close-mode is an emacs minor mode which provide a very simple
;;; yet efficient automatic closing parenthesis functionality.

(defun auto-close-paren (s)
  "Close parenthesis"
  `(lambda ()
     (interactive)
     (insert ',s)
     (goto-char (1- (point)))))

(defun auto-close-ignore (c)
  "Ignore c if next character is c"
  `(lambda ()
     (interactive)
     (let ((after (char-after (point))))
       (if (and after (= ',c after))
	   (goto-char (1+ (point)))
	 (insert-char ',c)))))

(defun auto-close-quote ()
  "Auto close quote and ignore the key if the next character is a quote"
  (interactive)
  (let ((c (char-after (point))))
    (if (and c (= ?\" c))
	(goto-char (1+ (point)))
      (progn 
	(insert "\"\"")
	(goto-char (1- (point)))))))

(defun auto-close-new-line ()
  "Defines behaviour when RETURN is pressed"
  (interactive)
  (let ((c (char-after (point))))
    (cond 
     ((and c (= ?\} c)) ; case {}
      (progn 
	(newline-and-indent)
	(newline-and-indent)
	(previous-line)
	(indent-according-to-mode)))
     (t ; else
      (progn 
	(newline)
	(indent-according-to-mode))))))

(defvar auto-close-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "(" (auto-close-paren "()"))
    (define-key map ")" (auto-close-ignore ?\)))

    (define-key map "[" (auto-close-paren "[]"))
    (define-key map "]" (auto-close-ignore ?\]))

    (define-key map "{" (auto-close-paren "{}"))
    (define-key map "}" (auto-close-ignore ?\}))

    (define-key map "\"" 'auto-close-quote)

    (define-key map (kbd "RET") 'auto-close-new-line)
    map)
  "Keymap for auto-close-mode minor mode")

(defvar auto-close-default-modes
  (list 
   'c-mode-common-hook
   'lisp-mode-hook
   'emacs-lisp-mode-hook
   'tuareg-mode-hook
   'coq-mode-hook)
  "List of major modes where auto-close-mode is enable by default")

(defun auto-close-autoload ()
  "Enable auto-close-mode for default major modes"
  (mapcar (lambda (mode) (add-hook mode (lambda () (auto-close-mode))))
	  auto-close-default-modes))

(auto-close-autoload)

(define-minor-mode auto-close-mode 
  "Close parenthesis and brackets automatically"
  :lighter " auto-close"
  :keymap auto-close-mode-keymap)

(provide 'auto-close-mode)
