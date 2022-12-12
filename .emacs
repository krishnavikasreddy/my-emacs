;; PACKAGES
;; (require 'package)
;; (package-initialize)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")             
;; (setq package-list (list 'org))


;; INIT CHECKS
(defvar aspell-warning-message-linux "aspell does not seems to be installed, this may cause orgmode flycheck to fail - apt install aspell")
(defvar aspell-warning-message-macos "aspell does not seems to be installed, this may cause orgmode flycheck to fail - brew install ispell")

;; aspell check

(defun mac-os-pre-checks ()
  (unless (file-exists-p "/usr/bin/aspell") (insert aspell-warning-message-macos))
)

(defun linux-os-pre-checks ()
 (unless (file-exists-p "/usr/bin/aspell") (insert aspell-warning-message-linux))
)


(if (eq system-type 'darwin) 'mac-os-pre-checks)
(if (eq system-type 'gnu/linux) 'linux-os-pre-checks)


;; BASICS
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(setq show-paren-delay 0)
(setq show-paren-style 'expression)

(show-paren-mode 1)
(electric-pair-mode 1)
(auto-revert-mode 1)
(visual-line-mode 1)
(subword-mode 1)

(setq inhibit-startup-message t)
(setq backup-directory-alist '(("." . "/tmp/")))

(global-set-key (kbd "C-c C-f") 'find-file-in-current-directory)
(define-key global-map (kbd "RET") 'newline-and-indent)

(setq-default indent-tabs-mode nil)                                                                                                                                                          
(setq-default tab-width 2)
                
            
(if window-system (progn (tool-bar-mode 0) (set-frame-size (selected-frame) 150 40)))
;; set the filename as buffer name
(setq frame-title-format "%b")
;; set the filename as buffer name
(setq frame-title-format "%b")


;; DEFAULTS
(setq-default explicit-shell-file-name "/bin/bash")
(if (file-directory-p (expand-file-name "~/miniconda3")) (setq python-shell-interpreter (expand-file-name "~/miniconda3/bin/python")) (insert "MINICONDA NOT FOUND"))




;;custom funcs
(defun save-all-and-compile ()
  (interactive)
  (save-some-buffers 1)
  (recompile))

(global-set-key (kbd "<f6>") 'compile)
(global-set-key [(f5)] 'save-all-and-compile)



;; ORG MODE
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
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename (concat (make-temp-name (concat (file-name-nondirectory (buffer-file-name)) "_" (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[./" filename "]]"))
  (org-display-inline-images))


(eval-after-load 'org '(color-keys-org))
(defun color-keys-org ()
  (interactive)
  (define-key org-mode-map (kbd "C-c M-r") '(lambda ()(interactive)(color-line "!")))
  (define-key org-mode-map (kbd "C-c M-y") '(lambda ()(interactive) (color-line "?")))
  (define-key org-mode-map (kbd "C-c M-g") '(lambda ()(interactive) (color-line "%")))
  )

;; add macros to the org file while opening so as to not to copy every time
(defvar ORG-MACROS-INIT
"#+HTML_HEAD: <link rel='stylesheet' type='text/css' href='https://cdn.jsdelivr.net/npm/bootstrap@3.3.7/dist/css/bootstrap.min.css' crossorigin='anonymous'/>
#+HTML_HEAD_EXTRA: <style>body{width:800px;margin:auto!important;line-height:1.5em;} </style>
#+MACRO: r @@html:<span class='text-danger'>@@$1@@html:</span>@@
#+MACRO: g @@html:<span class='text-success'>@@$1@@html:</span>@@
#+MACRO: y @@html:<span class='text-warning'>@@$1@@html:</span>@@"
) 

(defun add-macros-to-org () (unless (file-exists-p (buffer-file-name (current-buffer))) (insert ORG-MACROS-INIT)))

(add-hook 'org-mode-hook (lambda ()
			  (org-toggle-pretty-entities)
                          (add-macros-to-org)
                          (define-key org-mode-map (kbd "M-p") 'my-org-screenshot)
                          (flyspell-mode 1)))

(defun hook-export-org (regex c)
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
	    (hook-export-org "![^\s].+[^\s]!" "r")
	    (hook-export-org "\\%[^\s].+[^\s]\\%" "g")
	    (hook-export-org "\\?[^\s].+[^\s]\\?" "y")))

(defun color-line (c)
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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
                [default default default italic underline success warning error])
 '(ansi-color-names-vector
                ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes '(wheatgrass))
 '(ispell-dictionary nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

