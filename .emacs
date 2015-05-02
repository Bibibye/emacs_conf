; Add recursively ~/.emacs.d and its content to load-path
(let* ((dir (expand-file-name user-emacs-directory))
       (default-directory dir))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir t)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

;; Recompile every .elc
(byte-recompile-directory ".emacs.d/elpa" 0)

;; Customization
(custom-set-variables
 '(debug-on-error t)
 '(doc-view-continuous t)
 '(global-linum-mode 1)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((reftex-default-bibliography ".")))))
(load-theme 'custom-black t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode t)
(blink-cursor-mode 0)
(display-time-mode)
(column-number-mode)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq-default TeX-PDF-mode t)

;; markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)

;; Font size
(set-face-attribute 'default nil :height 120)

;; Full screen
(defun toggle-fullscreen ()
  "Enable fullscreen."
  (interactive)
  (when (or (eq window-system 'mac) (eq window-system 'x))
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key (kbd "C-c f") 'toggle-fullscreen) ; f as fullscreen

;; Html

(global-set-key (kbd "C-c b") 'browse-url-of-file)

;; If we are not in fullscreen mode, we toggle fullscreen
;; (when (not (frame-parameter nil 'fullscreen))
;;   (toggle-fullscreen))

;; Modes
(setq auto-mode-alist (append 
		       '(("\\.pl\\'" . prolog-mode)
			 ("\\.ys\\'" . asm-mode)
			 ("\\.S\\'" . asm-mode)
			 ("\\.hcl\\'" . shell-script-mode)
			 ("\\.y\\'" . bison-mode)
			 ("\\.l\{1,2\}\\'" . flex-mode)
			 ("\\.ll\\'" . flex-mode)
			 ("\\.php\\'" . php-mode)
			 ("\\.spl\\'" . spl-mode)
			 ("\\.mly\\'" . bison-mode)
			 ("\\.ml\\'" . tuareg-mode)
			 ("\\.mll\\'" . tuareg-mode)
			 ("\\.dot\\'" . c-mode)
			 ("\\.lisp\\'" . lisp-mode)
			 ("\\.cpp\\'" . c++-mode)
			 ("\\.hpp\\'" . c++-mode)
			 ("\\.h\\'" . c++-mode)
			 ("\\.md\\'" . markdown-mode)
			 )
		       auto-mode-alist))
;; Python
(setq py-python-command "python3")

;; Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(global-set-key (kbd "C-c C-k") 'prolog-compile-buffer)

;; Packages

;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")

;; Remove Yasnippet's default tab key binding
;; to avoid conflicts with auto-complete. Found at :
;; http://sethlakowske.com/why-i-use-emacs/fix-yasnippet-and-autocomplete-tab-key-collision/
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;;; The folowing code (everything about popups for YAS) comes from :
;;; http://www.emacswiki.org/emacs/Yasnippet

;;; More about popups? See https://github.com/auto-complete/popup-el

;;; use popup menu for yas-choose-value
(require 'popup)

;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

;; auto-complete-mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-source-yasnippet nil)
(global-auto-complete-mode t)

;; Column marker
;; 80 characters rule
(require 'column-marker)
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-3 80)))

;;; Packages list ;;;
(require 'cc-mode)
(require 'font-lock)
(require 'bison-mode)
(require 'flex-mode)
(require 'spl)

;; My packages
(require 'lambda-mode)
(require 'greek-mode)
(require 'auto-close-mode)
(require 'brainfuck-mode)
