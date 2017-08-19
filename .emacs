(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (adwaita)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(elpy-rpc-backend nil)
 '(elpy-rpc-python-command "python")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(man-notify-method (quote newframe))
 '(org-agenda-files
   (quote
    ("~/dev/catchMyNotes/src/TODO.org" "~/Documents/Schedule.org")))
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("!" diff-refine-removed)
     ("%" diff-refine-added)
     ("?" diff-refine-changed)
     ("_" Underline)
     ("=" Org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-hide-emphasis-markers t)
 '(package-selected-packages
   (quote
    (flycheck markdown-mode rjsx-mode json-mode js2-mode elpy yaml-mode web-mode simple-httpd python-environment org magit exec-path-from-shell epc)))
 '(show-trailing-whitespace t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil)
 '(version-control (quote never)))

;;dont display startup message
(setq inhibit-startup-message t)
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
            (elpy-enable)
            (set (make-local-variable 'compile-command)
                 (concat "python " buffer-file-name))
            (setq python-indent-offset 4)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (elpy-mode)
 	    ))

(add-hook 'js-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "node " buffer-file-name))
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
;;---------------------------------------

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
(add-hook 'hs-minor-mode-hook '(lambda ()
                                 (define-key hs-minor-mode-map (kbd "M-]") 'hs-toggle-hiding)
                                 (define-key hs-minor-mode-map (kbd "M-[") 'hs-toggle-hiding)
                                 ))

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

(global-set-key  [f1] (lambda () (interactive) (man (current-word))))
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "s-g") (lambda () (interactive) (imenu (thing-at-point 'symbol))))
(global-set-key (kbd "s-s") 'speedbar)
(global-set-key (kbd "s-k") 'kill-this-buffer)

;; backup files to .emacs-backup
(setq backup-directory-alist `(("." . "~/.emacs-backup")))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;set js indent level to 2
(setq js-indent-level 2)

;;keybinding for magit-status
(global-set-key (kbd "C-x g") 'magit-status)
(setq org-src-fontify-natively t)
(ido-mode 1)

;;enable sub-word mode
(global-subword-mode 1)

;; enable narrowing the region
(put 'narrow-to-region 'disabled nil)
(add-hook 'speedbar-load-hook (lambda ()
                                (speedbar-add-supported-extension ".js")
                                (speedbar-add-supported-extension ".jsx")
                                ))
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html

(add-to-list 'auto-mode-alist '("\\.jsx?" . rjsx-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other 
(exec-path-from-shell-initialize)
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun indent-close-tag-with-open (func &rest args)
  (apply func args)
  (save-excursion
    (let
        ((current-line (string-trim-left(buffer-substring
                                         (line-beginning-position)
                                         (line-end-position)))))
      (if (string-equal (substring current-line 0 1) ">")
          (progn
            (goto-char (line-beginning-position))
            (search-forward ">")
            (goto-char (- (point) 1))
            (delete-backward-char 2))
        (if (string-equal (substring current-line 0 2) "/>")
            (progn
              (goto-char (line-beginning-position))
              (search-forward "/>")
              (goto-char (- (point) 2))
              (delete-backward-char 2)))
        )
      )
    ))
(advice-add 'sgml-indent-line :around #'indent-close-tag-with-open)
(setq js-switch-indent-offset 2)
(setq js2-strict-trailing-comma-warning nil)

(global-visual-line-mode 1)
(add-to-list 'auto-mode-alist '("\\.html" . web-mode))
(yas-global-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
