(defvar logic-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "/\\" (lambda () (interactive) (insert-char ?a)))
    ;; (define-key map "\\" (lambda () (interactive) (insert-char ?)))
    ;; (define-key map ">" (insert-char ?>))
    ;; (define-key map "^" (insert-char ?n))
    ;; (define-key map "E" (insert-char ?∃))
    map)
  "Keymap for logic-mode minor mode")

(define-minor-mode logic-mode 
  "Edit logical proofs conveniently"
  :lighter " logic"
  :keymap logic-mode-keymap)

(provide 'logic-mode)
