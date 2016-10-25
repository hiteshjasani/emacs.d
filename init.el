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
                      ;; Structured editing of pairs (),{},[],<>
                      smartparens
                      ;; Enable moving buffers around the emacs frame
                      buffer-move
                      ;; Project file navigation
		      projectile
                      ;; Syntax highlighting for clojure
		      clojure-mode
                      ;; clojure/nrepl/emacs connector
		      cider
                      ;; markdown highlighting
                      markdown-mode
                      ;; M-x load-theme
                      color-theme-sanityinc-solarized
                      solarized-theme
                      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;------------------------------------------------------------
;; Configure buffers

;; Enables moving cursor between buffers using SHIFT and Arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Enables moving buffers within frame
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; Enable line/column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;;------------------------------------------------------------
;; Configure paths
(add-to-list 'exec-path "/usr/local/bin") ; for cider-jack-in

;;------------------------------------------------------------
;; Configure smartparens
(require 'smartparens-config)
(add-hook 'html-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'smartparens-mode)
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)


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

;;------------------------------------------------------------
;; Configure custom variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
