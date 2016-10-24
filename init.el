;; Package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;; Create repository index, only when needed
(unless package-archive-contents (package-refresh-contents))

;; User selected packages to install
(defvar my-packages '(
                      ;; Sets up ido-mode, uniquify and some base defaults
                      better-defaults
                      ;; Structured editing of s-expressions
                      buffer-move
                      ;; Project file navigation
                      paredit
                      ;; Enable moving buffers around the emacs frame
		      projectile
                      ;; Syntax highlighting for clojure
		      clojure-mode
                      ;; clojure/nrepl/emacs connector
		      cider
                      ;; markdown highlighting
                      markdown-mode
                      color-theme-sanityinc-solarized
                      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;------------------------------------------------------------
;; Configure Wind Move
;; Enables moving cursor between buffers using SHIFT and Arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;------------------------------------------------------------
;; Configure buffer-move
;; Enables moving buffers within frame
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;;------------------------------------------------------------
;; Configure paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;------------------------------------------------------------
;; Configure clojure
; vertically align clojure forms when selected and hit TAB
(setq clojure-align-forms-automatically t)

;;------------------------------------------------------------
;; Configure markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; -------------------------------------------------------------------
;; Customizations

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
