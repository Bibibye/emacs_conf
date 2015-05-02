(defvar brainfuck-mode-hook nil)

(defvar brainfuck-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'brainfuck-eval-line)
    (define-key map (kbd "C-c C-b") 'brainfuck-eval-buffer)
    (define-key map (kbd "C-c C-r") 'brainfuck-reset-machine)
    map)
  "Keymap for Brainfuck major mode")

(add-to-list 'auto-mode-alist '("\\.bf\\'" . brainfuck-mode))

(defconst brainfuck-font-lock-keywords nil)

;; Machine definition

(defvar brainfuck-memory nil)
(defvar brainfuck-pointer 0)
(defvar brainfuck-stack nil)

(defun brainfuck-reset-machine ()
  (interactive)
  (setq brainfuck-memory (make-vector 30000 0))
  (setq brainfuck-pointer 0)
  (setq brainfuck-stack nil))

;; Brainfuck evaluation

(defun brainfuck-display-output ()
  (let ((out-buf (get-buffer-create "*brainfuck-output*"))
	(out-win (display-buffer "*brainfuck-output*")))
    (select-window out-win)))

(defun brainfuck-change-value (new-value)
  (aset brainfuck-memory brainfuck-pointer new-value))

(defun brainfuck-stack-push (p)
  (setq brainfuck-stack (cons p brainfuck-stack)))

(defun brainfuck-stack-pop ()
  (setq brainfuck-stack (cdr brainfuck-stack)))

(defun brainfuck-stack-top ()
  (car brainfuck-stack))

(defun brainfuck-end-loop (code pc)
  (let ((origin pc))
    (setq pc (1+ pc))
    (while (not (and
		 (= (elt code pc) ?\])
		 (or (eq nil (brainfuck-stack-top))
		     (eq origin (brainfuck-stack-top)))))
      (cond
       ((= ?\[ (elt code pc)) (brainfuck-stack-push pc))
       ((= ?\] (elt code pc)) (brainfuck-stack-pop))
       (t nil))
      (setq pc (1+ pc)))
    pc))

(defun brainfuck-eval-code (code)
  (interactive)
  (let ((pc 0))
    (while (> (string-width code) pc)
      (let ((inst (elt code pc))
	    (curr (elt brainfuck-memory brainfuck-pointer)))
	(cond
	 ((= ?\+ inst) (brainfuck-change-value (1+ curr)))
	 ((= ?\- inst) (brainfuck-change-value (1- curr)))
	 ((= ?\> inst) (setq brainfuck-pointer (1+ brainfuck-pointer)))
	 ((= ?\< inst) (setq brainfuck-pointer (1- brainfuck-pointer)))
	 ((= ?\. inst) (insert-char curr))
	 ((= ?\! inst) (insert-string (int-to-string curr)))
	 ((= ?\, inst) (brainfuck-change-value (read-char "Input: ")))
	 ((= ?\? inst) (brainfuck-change-value (floor 
						(string-to-number 
						(read-string "Input: ")))))
	 ((= ?\] inst) (if (= curr 0)
			   (brainfuck-stack-pop)
			 (setq pc (brainfuck-stack-top))))
	 ((= ?\[ inst) (if (= curr 0)
			   (setq pc (brainfuck-end-loop code pc))
			 (brainfuck-stack-push pc)))
	 (t nil))
	(setq pc (1+ pc)))))
  nil)

(defun brainfuck-eval (code)
  (interactive)
  (let ((sw (selected-window)))
    (brainfuck-display-output)
    (brainfuck-eval-code code)
    (select-window sw)))

(defun brainfuck-eval-line ()
  (interactive)
  (let ((code (buffer-substring-no-properties
	       (line-beginning-position)
	       (line-end-position))))
    (brainfuck-eval code)))

(defun brainfuck-eval-buffer ()
  (interactive)
  (let ((code (buffer-string)))
    (brainfuck-eval code)))

;; Menu bar
;; See : http://ergoemacs.org/emacs/elisp_menu.html
;; for more infos

(defun brainfuck-init-menu-bar ()
  (define-key-after
    global-map
    [menu-bar bf-menu]
    (cons "Brainfuck" (make-sparse-keymap "foo"))
    'tools)
  (define-key
    global-map
    [menu-bar bf-menu bf-rm]
    '("Reset Machine" . brainfuck-reset-machine))
    (define-key
    global-map
    [menu-bar bf-menu bf-eb]
    '("Eval Buffer" . brainfuck-eval-buffer))
  (define-key
    global-map
    [menu-bar bf-menu bf-el]
    '("Eval Line" . brainfuck-eval-line)))

(define-derived-mode brainfuck-mode fundamental-mode "Brainfuck"
  "Major mode for editing Brainfuck code."
  (set (make-local-variable 'font-lock-defaults) '(brainfuck-font-lock-keywords))
  (brainfuck-init-menu-bar)
  (brainfuck-reset-machine))

(provide 'brainfuck-mode)
