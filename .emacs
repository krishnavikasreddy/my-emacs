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
(if window-system (progn
		    (tool-bar-mode 0)
		    (desktop-save-mode 1)
		    ))

;; set the filename as buffer name
(setq frame-title-format "%b")
(setq vc-follow-symlinks 1)

(setq backup-directory-alist `(("." . "/tmp")))
(set-fringe-mode 0)
(global-visual-line-mode 1)
;;====================================================================================================
;; ORG mode
;;(org-set-local 'font-lock-global-modes (list 'not major-mode))
(setq org-hide-emphasis-markers t)
(defface my-face-success '((t :foreground "black" :background "green" )) nil)
(defface my-face-warning '((t :foreground "white" :background "yellow" )) nil)
(defface my-face-error '((t :foreground "white" :background "red" )) nil)

(setq org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("!" my-face-error)
     ("%" my-face-success)
     ("?" my-face-warning)
     ("_" Underline)
     ("=" Org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))

(setq org-startup-with-inline-images t)

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[./" filename "]]"))
  (org-display-inline-images))


(eval-after-load 'org '(color-keys-org))
(defun color-keys-org ()
  (interactive)
  (define-key org-mode-map (kbd "C-c M-r") '(lambda
					      ()(interactive)
					      (color-red "!")))
  (define-key org-mode-map (kbd "C-c M-y") '(lambda
					      ()(interactive)
					      (color-red "?")))
  (define-key org-mode-map (kbd "C-c M-g") '(lambda
					      ()(interactive)
					      (color-red "%")))
  )

;; add macros to the org file while opening so as to not to copy every time
(defun add-macros-to-org ()
  (unless (file-exists-p (buffer-file-name (current-buffer)))
    (insert "#+HTML_HEAD: <link rel='stylesheet' type='text/css' href='/home/krishna/Documents/bootstrap.css' />
#+HTML_HEAD_EXTRA: <style>body{width:800px;margin:auto!important;line-height:1.5em;} </style>
#+MACRO: r @@html:<span class='text-danger'>@@$1@@html:</span>@@
#+MACRO: g @@html:<span class='text-success'>@@$1@@html:</span>@@
#+MACRO: y @@html:<span class='text-warning'>@@$1@@html:</span>@@
")))

(add-hook 'org-mode-hook (lambda ()
			  (org-toggle-pretty-entities)
                          (add-macros-to-org)
                          (define-key org-mode-map (kbd "M-p") 'my-org-screenshot)
                          (flyspell-mode 1)))

(defun vikas-export-org (regex c)
  (goto-line 0)
  (while (re-search-forward regex nil t)
    (goto-char (match-beginning 0))
    (delete-forward-char 1)
    (insert (concat "{{{" c "( "))
    (goto-char (+ (match-end 0) 5))
    (delete-backward-char 1)
    (insert " )}}}")
    )
  )
(add-hook 'org-export-before-processing-hook
	  (lambda (b)
	    (vikas-export-org "![^\s].+[^\s]!" "r")
	    (vikas-export-org "\\%[^\s].+[^\s]\\%" "g")
	    (vikas-export-org "\\?[^\s].+[^\s]\\?" "y")))

(defun color-red (c)
  (interactive "s")
  (save-excursion
    (if (region-active-p)
	(progn
          (let ((x (region-beginning)) (y (region-end)))
	  (goto-char x)
          (insert c)
          (goto-char (+ y 1))
          (insert c)
	  ))
      (progn
	(goto-char (org-beginning-of-item))
	(search-forward "-")
	(goto-char (+ (point) 1))
	(insert c)
	(goto-char (line-end-position))
	(insert c)
	))))

(setq org-src-fontify-natively t)



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
;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
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
(setq show-trailing-whitespace t)
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (flycheck company-mode yasnippet swiper ivy-mode django-snippets django-commands django-mode magit idomenu exec-path-from-shell elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'dired-find-alternate-file 'disabled nil)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(setq ediff-split-window-function #'split-window-horizontally)
