(setq inhibit-startup-message 1)

;;====================================================================================================
;; PACKAGES
;; add melpa to packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-list
      (list 'yasnippet
	    'exec-path-from-shell
	    'idomenu
	    'iedit
	    'org
	    'magit
	    'company
            'company-anaconda
            'anaconda-mode
	    'django-mode
	    'django-commands
	    'django-snippets
            'ivy
            'counsel
            'swiper
	    'json-mode))

;; install packages if not present
(dolist (package package-list)
  (unless (package-installed-p package) (package-install package)))


;; install python flycheck dependencies
(progn
  (let*
    ((x (shell-command-to-string "pydoc flake8")))
  (if (string-match "no Python documentation found for 'flake8'" x)
      (shell-command "pip install --user flake8"))))
;;====================================================================================================
;; MODES
;; match brackets
(progn
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression)
  (show-paren-mode 1))

(electric-pair-mode 1)
(auto-revert-mode 1)
(visual-line-mode 1)
(ido-mode 1)
(subword-mode 1)
(winner-mode 1)
(setq indent-tabs-mode nil)
;; ivy mode
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;;====================================================================================================

;; disable toolbar
(if window-system (tool-bar-mode 0))

;; set the filename as buffer name
(setq frame-title-format "%b")
(setq vc-follow-symlinks 1)

(setq backup-directory-alist `(("." . "/tmp")))
;;====================================================================================================
;; KEYS
(global-set-key (kbd "<f6>") 'compile)
(global-set-key [(f5)] 'save-all-and-compile)
(global-set-key (kbd "C-c C-f") 'find-file-in-current-directory)
;; auto indent after ret key
(define-key global-map (kbd "RET") 'newline-and-indent)

;; these are a fix for elpy bugs
(define-key global-map (kbd "C-c o") 'iedit-mode)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;;====================================================================================================
;; FUNCTIONS
;; save and compile
(defun save-all-and-compile ()
  (interactive)
  (save-some-buffers 1)
  (recompile))

;;====================================================================================================
;; HOOKS
(add-hook 'after-init-hook 'global-company-mode)


;;====================================================================================================
;; MODE SPECIFIC ACTIONS
;; all programming modes
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(eval-after-load "company"
 '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

(add-hook 'prog-mode-hook 'hs-minor-mode 1)
(add-hook 'hs-minor-mode-hook '(lambda ()
                                 (define-key hs-minor-mode-map (kbd "M-]") 'hs-toggle-hiding)
                                 (define-key hs-minor-mode-map (kbd "M-[") 'hs-toggle-hiding)
                                 ))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (whitespace-mode)
	    (yas-minor-mode)
	    (define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
	    ))

;; python mode
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "python " buffer-file-name))
            (setq python-indent-offset 4)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (anaconda-mode)
            (anaconda-eldoc-mode)
	    (flycheck-mode)
 	    ))

(setq json-reformat:indent-width 1)

;;====================================================================================================
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
(exec-path-from-shell-copy-env "CUDA_HOME")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck company-mode yasnippet swiper ivy-mode django-snippets django-commands django-mode magit idomenu exec-path-from-shell elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
