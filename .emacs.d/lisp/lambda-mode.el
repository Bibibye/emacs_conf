(defvar lambda-mode-hook nil)

(defvar lambda-mode-map
  (let ((map (make-keymap)))
    (define-key map "\\" 'lambda-insert)
    (define-key map "(" 'lambda-close-paren)
    map)
  "Keymap for lambda calculus major mode")

(add-to-list 'auto-mode-alist '("\\.lbd" . lambda-mode))

(defconst lambda-font-lock-keywords
  (list
   '("[\u03bb\.]" . 
     font-lock-builtin-face)
   '("[a-zA-Z0-9]+" .
     font-lock-variable-name-face)))

(defvar lambda-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\u03bb "w" st)
    (modify-syntax-entry ?\. "w" st)
    (modify-syntax-entry ?\( "(" st)
    (modify-syntax-entry ?\) ")" st)
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\; "." st)
    st)
  "Syntax table for lambda-mode")

(defun lambda-indent-line ()
  "Indent current line as lambda calculus code"
  (interactive)
  (indent-line-to 0)
)

(defun lambda-close-paren ()
    (interactive)
    (insert "()")
    (goto-char (1- (point))))

(defun lambda-insert ()
  "Insert lambda unicode character λ."
  (interactive)
  (insert-char ?\u03bb))

(define-derived-mode lambda-mode fundamental-mode "Lambda Calculus"
  "Major mode for editing lambda calculus expressions."
  (set (make-local-variable 'font-lock-defaults) '(lambda-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'lambda-indent-line))

(provide 'lambda-mode)
