;; This is a line to understand the complexities of branching
;; new branch and new things ahooo
(setq horizontal-scroll-bar-mode 'disabled)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote newframe))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (misterioso)))
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T)
     (ess-R-fl-keyword:%op%))))
 '(org-agenda-files (quote ("~/Documents/Schedule.org")))
 '(org-emphasis-alisty
   (quote
    (("*" bold)
     ("/" italic)
     ("!"
      (:foreground "red"))
     ("%"
      (:foreground "green"))
     ("?"
      (:foreground "yellow"))
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-hide-emphasis-markers t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;dont display startup message
(setq inhibit-startup-message t)

;line numbers
(global-linum-mode 1)


;key shortcuts
(global-set-key [(C-f5)] 'compile)
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

;;HOOKS
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "python " buffer-file-name))
			(setq python-indent-offset 4)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)
	    (jedi:setup)
		(setq jedi:setup-keys t)
	    ))


(add-hook 'js-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "node " buffer-file-name))
	    (hs-minor-mode t)
	    ))

;; this is for the disappearence of the top menu bar
(tool-bar-mode -1)

;;ORG-MODE variables
(setq org-startup-with-inline-images t)


;;Line numbers
(global-visual-line-mode t)


;;-------------------------------------------
;; this is for taking screen shot
;;TODO ;figure out a way to add it only to org-mode
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
(global-set-key (kbd "M-p") 'my-org-screenshot)
;;------------------------------------------


;;Display error messages-buffer
(setq debug-on-error t)

;; to open previous configuration
(desktop-save-mode 1)



;; turn on highlight matching brackets when cursor is on one
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode 1)

;;putiing auto pairs automatically
(electric-pair-mode 1)
;;add <> in auto complete mode in sgml and nxml mode-line



;; adding melpa package archive
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; all your custom scripts are stored in this path
(add-to-list 'load-path "~/.emacs.d/lp/")

;; adding html-tags mode this is to identify the tags pair automatically
(require 'hl-tags-mode)
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))


(defun vikas ()
  (interactive)
  (save-excursion
   (setq char-in (search-backward "<" nil t))
  (forward-char 1)
  (setq mystr (thing-at-point 'symbol)))
  (save-excursion
    (setq char-not-in (search-backward "/" nil t)))
  (if (equal char-in nil) (setq char-in -1))
  (if (equal char-not-in nil) (setq char-not-in -1))
  (if (> char-in char-not-in)
      (progn

	(newline-and-indent)
	(insert (concat "\n</" mystr ">"))
	(indent-according-to-mode)
	(forward-line -1)
	(indent-according-to-mode)
      )))


(defun add-key-close()
  (local-set-key (kbd "C-.") 'vikas))

(add-hook 'sgml-mode-hook 'add-key-close)




;;typescript mode
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))


;; Smooth Scrolling
;; scroll one line at a time (less "jumpy" than defaults)
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

(defun test-hook (change-beg change-end prev-len)
  (if (and (> change-beg start-s) (<= change-end start-e))
      (save-excursion
	(goto-char start-s)
	(setq-local cur-w (thing-at-point 'word))
	(goto-char (+ end-s 3))
	(delete-region (+ end-s 3) end-e)
	(insert "h")
	)
  ))

(defun auto-change-html (change-beg change-end prev-len)
  (if (not (equal cur-w 'nil))
      (if (not (equal (car cur-w) (cdr cur-w)))
	  (progn
	    (setq start-s (car (car cur-w)))
	    (setq start-e (cdr (car cur-w)))
	    (setq end-s (car (cdr cur-w)))
	    (setq end-e (cdr (cdr cur-w)))

  (if (and (<= (point) start-e) (> (point) start-s))
    (save-excursion
      (goto-char (+ start-s 1))
      (setq cur-w (buffer-substring (point) (- (search-forward ">") 1)))
      (if (= prev-len 1)
      (progn
	(delete-region (+ end-s 1) (- end-e 2))
	(goto-char (+ end-s 1))
	(insert cur-w)
	)
     (progn
	(delete-region (+ end-s 3) end-e)
	(goto-char (+ end-s 3))
	(insert cur-w)
	)
      )
      ))
    (if (and (< (point) end-e) (> (point) end-s))
    (progn
      (setq cur-w (hl-tags-context))
      (setq start-s (car (car cur-w)))
      (setq start-e (cdr (car cur-w)))
      (setq end-s (car (cdr cur-w)))
      (setq end-e (cdr (cdr cur-w)))

	(save-excursion
	  (goto-char (+ end-s 2))
	  (setq cur-w (buffer-substring (point) (- (search-forward ">") 1)))
	  (delete-region (+ start-s 1) (- start-e 1))
	  (goto-char (+ start-s 1))
	  (insert cur-w)
	  ))
      )))))


(defun auto-change-html-before-change (begin end)
 (setq cur-w (hl-tags-context)))

(setq inhibit-modification-hooks nil)
;;(add-hook 'after-change-functions 'test-hook)


;; (add-hook 'sgml-mode-hook
;;           (lambda ()
;; 	    (add-hook 'after-change-functions 'auto-change-html nil 'make-it-local)
;; 	    (add-hook 'before-change-functions 'auto-change-html-before-change nil 'make-it-local)
;; 	    ))


(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))
;;------------------------------------------
;;hideshow  code toggling
(eval-after-load "hideshow" '(define-key hs-minor-mode-map (kbd "C-c C-c") 'hs-toggle-hiding))

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

(add-hook 'org-mode-hook (lambda ()
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


;;-----------
;; php-mode
(require 'php-mode);

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


;;; Auto-Complete-Mode

(package-initialize)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20160827.649/dict")
(ac-config-default)

(global-set-key  [f1] (lambda () (interactive) (manual-entry (current-word))))


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

;;---python jedi

(setq ac-auto-start 0)
(setq ac-auto-show-menu 0)
(setq ac-delay 0)
(setq ac-quick-help-delay 0)
(setq ac-quick-help-prefer-postip 1)


(setq jedi:get-in-function-call-delay 0)
(setq jedi:doc-mode t)
(setq jedi:complete-on-dot t)
;;(org-agenda nil "n")


(setq ess-help-own-frame t)

;; indentation guide
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\.)





(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
