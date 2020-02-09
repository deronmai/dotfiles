;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   ;;'("melpa" . "http://melpa.milkbox.net/packages/")
   ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
   t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
  )

(setq package-enable-at-startup t)
(package-initialize)

;;(setq package-check-signature nil)


;; Evil Mode
(add-to-list 'load-path "~/.emacs.d/evil")
(use-package evil
  :ensure t
  :config (evil-mode))


;; line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; no welcome screen
(setq inhibit-startup-screen t)


(require 'ace-jump-mode)    ; better vesion of ez motion
(require 'ansi-color)


;;--------------------------
;; Shell
;;--------------------------
;; attempt to make shell mode less bad
(use-package bash-completion
  :config (bash-completion-setup))
; Make the shell look pretty
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")

(defun tshell()
  (interactive)
  (setq new-shell-name (read-from-minibuffer "shell buffer name: " nil nil nil nil "*shell*"))
  (shell)
  (rename-buffer new-shell-name))
;;(evil-define-key 'normal 'global (kbd "SPC tm") 'tshell)



;;
(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  ;(setq telephone-line-lhs
  ;      '((evil   . (telephone-line-evil-tag-segment))
  ;        (nil    . (telephone-line-minor-mode-segment
  ;                   telephone-line-buffer-segment))))
  ;(setq telephone-line-rhs
  ;      '((nil    . (telephone-line-misc-info-segment))
  ;        (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode 1))

; compilation stuff
(setq compilation-scroll-output t)
; colorize the compilation output
(defun colorize-compilation-buffer()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;;--------------------------
;; emacs setup
;;--------------------------
(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;--------------------------
;; Generic rebindings
;; (define-key evil-motion-state-map (kbd <key>) <command>)
;; (define-key evil-normal-state-map (kbd <key>) <command>)
;;--------------------------
(define-key evil-motion-state-map (kbd "SPC") nil)

;;--------------------------
;; Navigation
;;--------------------------
(evil-define-key '(normal) 'global (kbd "SPC SPC") 'ace-jump-mode)

;;--------------------------
;; Visualization/formatting
;;--------------------------
;; Highlight whitespace and lines > 160 chars
;;(global-whitespace-mode t)
(setq-default whitespace-line-column 160)
;; Theme (zenburn)
;; (load-theme 'zenburn t)
;; Theme gruvbox
(load-theme 'gruvbox t)
(menu-bar-mode 1)
; indentation shit
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)
(setq-default tab-width 4)
(menu-bar-mode -1)
(tool-bar-mode -1)


;;--------------------------
;;           HELM
;;--------------------------
;; installed via melpa
(use-package helm
  :ensure t
  :config (helm-mode))
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(evil-define-key '(normal motion) 'global "\M-x" 'helm-M-x)
(define-key evil-normal-state-map (kbd "SPC d f") 'helm-find-files)
(helm-mode 1)

;;--------------------------
;;           ORG
;;--------------------------
(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (lambda ()
                                   (org-bullets-mode 1))))
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;;(eval-after-load 'org '(require 'org-pdfview))
(evil-define-key 'normal 'global (kbd "SPC c") 'org-cycle)

;;--------------------------
;;           Func
;;--------------------------
;; message the last time this file was modified
(defun lt()
  (interactive)
  (message (format-time-string "%D %H:%M:%S" (visited-file-modtime))))

(defun lcd()
  (interactive)
  (cd (file-name-directory (buffer-file-name))))

;;--------------------------
;;           Emacs-lsp
;;--------------------------
(use-package lsp-mode
  :ensure t)
(require 'lsp-clients)
;;(require 'lsp-mode)
;;(require 'lsp-clients)
;;(add-hook 'c++-mode-hook #'lsp)
(require 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;;--------------------------
;;           Autocomplete
;;--------------------------
(use-package company
  :ensure t
  :config (global-company-mode)
  (setq company-idle-delay 0.0))


;; leetcode
;;(setq leetcode-prefer-sql "c++	")
(require 'leetcode)

;;--------------------------
;;       C++ Specific
;;--------------------------

;; clang-format
;;(require 'clang-format)
;;(setq clang-format-style-option "llvm")
(use-package clang-format
  :ensure t)
(evil-define-key 'normal 'global (kbd "SPC ff") 'clang-format-buffer)
(evil-define-key 'normal 'global (kbd "SPC fr") 'clang-format-region-at-point)

;;(defun clang-format-region-at-point()
;;  (interactive)
;;  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
;;    (clang-format-region (car bounds) (cdr bounds))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-lsp company bash-completion ace-jump-mode elpy evil-magit magit org-bullets use-package org clang-format gruvbox-theme furl graphql-mode dash leetcode zenburn-theme lsp-ui lsp-mode helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
