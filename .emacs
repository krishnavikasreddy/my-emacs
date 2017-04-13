(package-initialize)
(semantic-mode 1)
(set-face-attribute 'default nil :height 105)
(setq-default line-spacing 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(company-tern-meta-as-single-line t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 0)
 '(company-tooltip-minimum 6)
 '(company-tooltip-minimum-width 40)
 '(custom-enabled-themes nil)
 '(ecb-options-version "2.50")
 '(ecb-source-path (quote (("/" "/"))))
 '(global-company-mode t)
 '(man-notify-method (quote newframe))
 '(org-agenda-files
   (quote
    ("~/dev/catchMyNotes/src/TODO.org" "~/Documents/Schedule.org")))
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("!"
      (:foreground "red"))
     ("%"
      (:Foreground "Green"))
     ("?"
      (:Foreground "Yellow"))
     ("_" Underline)
     ("=" Org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-hide-emphasis-markers t)
 '(package-selected-packages
   (quote
    (rjsx-mode simple-httpd python-environment pos-tip org magit flycheck exec-path-from-shell epc company-tern company-statistics company-shell company-dict company-c-headers company-anaconda)))
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 105 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(mode-line ((t (:background "white" :foreground "black"))))
 '(mode-line-inactive ((t (:background "black" :foreground "#eeeeec"))))
 '(show-paren-match ((t (:inherit (quote region))))))

;;dont display startup message
(setq inhibit-startup-message t)

;;line numbers
(global-linum-mode 1)

;;key shortcuts
(global-set-key (kbd "<f6>") 'compile)
(global-set-key [(f5)] 'save-all-and-compile)

;; save and compile
(defun save-all-and-compile ()
  (interactive)
  (save-some-buffers 1)
  (recompile))


;;auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;;set the file name as buffer
(setq frame-title-format "%b")

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "python " buffer-file-name))
            (anaconda-mode 1)
            (anaconda-eldoc-mode 1)
            (setq python-indent-offset 4);
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)
	    ))

(add-to-list 'load-path "~/projects/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "node " buffer-file-name))
            (tern-mode t)
	    ))

;; this is for the disappearence of the top menu bar
(if window-system (tool-bar-mode 0))

;;ORG-MODE variables
(setq org-startup-with-inline-images t)

;; tabs - spaces
(setq-default indent-tabs-mode nil)
;;-------------------------------------------
;; this is for taking screen shot
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
;;------------------------------------------


;;Display error messages-buffer
;;(setq debug-on-error t)

;; to open previous configuration
;; (desktop-save-mode 1)



;; turn on highlight matching brackets when cursor is on one
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode 1)

;;putiing auto pairs automatically
(electric-pair-mode 1)
(push '(?\' . ?\') electric-pair-pairs)
(push '(?\' . ?\') electric-pair-text-pairs)
;;add <> in auto complete mode in sgml and nxml mode-line



;; adding melpa package archive
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; all your custom scripts are stored in this path
;; (add-to-list 'load-path "~/.emacs.d/lp/")

;; Smooth Scrolling
;; scroll one line at a time (less "jumpy" than defaults)
;;(horizontal-scroll-bar-mode -1)
(scroll-bar-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


;; refresh files as it is changed in the disk
(global-auto-revert-mode t)

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [backspace]
              (lambda () (interactive) (find-alternate-file "..")))
                                        ; was dired-up-directory
            ))
;;------------------------------------------
;;hideshow  code toggling
(add-hook 'prog-mode-hook 'hs-minor-mode 1)
(add-hook 'hs-minor-mode-hook '(lambda ()(define-key hs-minor-mode-map (kbd "C-c C-c") 'hs-toggle-hiding)))

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
                          (add-macros-to-org)
                          '(define-key org-mode-map (kbd "M-p") 'my-org-screenshot)
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
	    (vikas-export-org "\\?[^\s].+[^\s]\\?" "y")
	    ))



(defun color-red (c)
  (interactive "s")
  (save-excursion
    (goto-char (org-beginning-of-item))
    (search-forward "-")
    (goto-char (+ (point) 1))
    (insert c)
    (goto-char (line-end-position))
    (insert c)
    )
  )


;;twittering-mode
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
;;(setq twittering-reverse-mode t)
(setq twittering-use-icon-storage t)

;;R
(setq ess-help-reuse-window t)
;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))


(global-set-key  [f1] (lambda () (interactive) (man (current-word))))


(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "<f2>n") 'next-buffer)
(global-set-key (kbd "<f2>p") 'previous-buffer)

(global-set-key (kbd "<f2><up>") 'windmove-up)
(global-set-key (kbd "<f2><down>") 'windmove-down)
(global-set-key (kbd "<f2><left>") 'windmove-left)
(global-set-key (kbd "<f2><right>") 'windmove-right)

(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

(global-set-key (kbd "s-g") (lambda () (interactive) (imenu (thing-at-point 'symbol))))
(global-set-key (kbd "s-s") 'speedbar)
(global-set-key (kbd "C-0") 'delete-window)

;; backup files to .emacs-backup
(setq backup-directory-alist `(("." . "~/.emacs-backup")))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;set js indent level to 2
(setq js-indent-level 2)


;;keybinding for magit-status
(global-set-key (kbd "C-x g") 'magit-status)
(setq org-src-fontify-natively t)

(ido-mode 1)
(if window-system
(setq-default
 mode-line-format
 (list
  " "'(:eval (propertize (format-time-string "%I:%M:%p") 'face '(:foreground "red" :weight bold)))
  " "'(:eval (propertize "%b"
                         'face '(:weight bold :background "blue" :foreground "white")
                         'help-echo (buffer-file-name)))
  '(vc-mode vc-mode)
  " "'(:eval (propertize "%m" 'face '(:foreground "dark green" :weight bold) 'help-echo buffer-file-coding-system))
  " "'(:eval
       (cond
        (buffer-read-only
         (propertize "READ ONLY"
                     'face '(:foreground "red" :weight bold :background "black")
                     'help-echo "buffer is read-only"))
        ((buffer-modified-p)
         (propertize "MODIFIED"
                     'face '(:foreground "white" :weight bold :background "red")
                     'help-echo "buffer modified."))

        )
       )
  )))


;; ;; set the split to horizontal rather than vertical
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)

;; open buffer list on mouse-3 button
(global-set-key [down-mouse-3] 'mouse-buffer-menu)
(global-flycheck-mode 1)
;; turn on flychecking globally
(add-hook 'prog-mode-hook #'global-flycheck-mode)


;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;;use eslint with mode for jsx files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;;enable sub-word mode
(global-subword-mode 1)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint)))
  )
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(setq linum-supress-updates t)
(global-company-mode 1)
(setq company-dabbrev-downcase nil)
(add-to-list 'company-backends '(company-tern company-shell company-anaconda company-c-headers))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook '(lambda ()
                            (tern-mode 1)))
(add-hook 'tern-mode-hook '(lambda ()
                             (define-key tern-mode-keymap (kbd "s-h") 'tern-highlight-refs)))
