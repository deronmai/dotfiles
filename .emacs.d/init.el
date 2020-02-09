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


(require 'ansi-color)

;;--------------------------
;; Generic rebindings
;; (define-key evil-motion-state-map (kbd <key>) <command>)
;; (define-key evil-normal-state-map (kbd <key>) <command>)
;;--------------------------
(define-key evil-motion-state-map (kbd "SPC") nil)

;;--------------------------
;;           HELM
;;--------------------------
;; installed via melpa
(require 'helm-config)
;;(use-package helm
;;  :ensure t
;;  :config (helm-mode))
;; (global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(evil-define-key '(normal motion) 'global "\M-x" 'helm-M-x)
(define-key evil-normal-state-map (kbd "SPC df") 'helm-find-files)
(helm-mode 1)

;; Emacs-lsp
;;(use-package lsp-mode :commands lsp :ensure t)
(require 'lsp-mode)
;;(add-hook 'c++-mode-hook #'lsp)
(require 'lsp)
(require 'lsp-clients)
(add-hook 'c++-mode-hook 'lsp)

;; leetcode
;;(setq leetcode-prefer-sql "c++	")
(require 'leetcode)


;; Theme (zenburn)
;; (load-theme 'zenburn t)
;; Theme gruvbox
(load-theme 'gruvbox t)


;;--------------------------
;;       C++ Specific
;;--------------------------

;; clang-format
(require 'clang-format)
(setq clang-format-style-option "llvm")
;;lisp
(evil-define-key 'normal 'global (kbd "SPC fr") 'clang-format-region-at-point)
(evil-define-key 'normal 'global (kbd "SPC ff") 'clang-format-buffer)
;;lisp
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
    (ace-jump-mode elpy evil-magit magit org-bullets use-package org clang-format gruvbox-theme furl graphql-mode dash leetcode zenburn-theme lsp-ui lsp-mode helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
