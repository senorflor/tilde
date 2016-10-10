;;; Slather with elisp
(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-archive-enable-alist '(("melpa" deft magit)))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                   ;; General
                      dash
                      org
                      rainbow-delimiters
                      undo-tree

                   ;; Formats
                      markdown-mode
                      yaml-mode

		   ;; Java
		      ;; malabar-mode
		      groovy-mode

                   ;; Clojure
                      ac-cider
                      cider
                      clojure-mode
                      clojurescript-mode
		      paredit

                   ;; Go
                      go-mode

                   ;; Haskell
                      haskell-mode

                   ;; Javascript
                      jsx-mode

		    ;; Elm
		      elm-mode

                   ;; Project nav
                      projectile)
  "Packages required at launchtime")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; global setup
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(delete-selection-mode t)
(put 'downcase-region 'disabled nil)
(setq-default tab-width 2
	      standard-indent 2
	      indent-tabs-mode nil)

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;; rainbow parens
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; Java: 2-space indent
(add-hook 'java-mode-hook
	  (lambda ()
	    (setq
	     c-basic-offset
	     2)))
;; (require 'malabar-mode)
;; (load-file "~/projects/cedet/cedet-devel-load.el")
;; (add-hook 'after-init-hook (lambda ()
;; 			     (message "activate-malabar-mode")
;; 			     (activate-malabar-mode)))

;; (add-hook 'malabar-java-mode-hook 'flycheck-mode)
;; (add-hook 'malabar-groovy-mode-hook 'flycheck-mode)

;;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
;; Clojurescript/EDN highlighting
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))  ; *.edn are Clojure files
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))
;;; cider config
(require 'ac-cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(setq nrepl-hide-special-buffers t)
(setq nrepl-log-messages t)
(setq cider-show-error-buffer 'only-in-repl)

;;; Javascript
(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'js-custom)
;; it's just JSX, don't overreact
(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;;; org mode!
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; auto-complete
(if (equal nil (boundp 'ac-dictionary-directories))
    (setq ac-dictionary-directories '()))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict" t)
(require 'auto-complete-config)
(ac-config-default)

;;; solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)

;;; haskell
(add-hook 'haskell-mode-hook
          (lambda () (interactive)
            (local-set-key (kbd "TAB") (kbd "SPC SPC"))
            (kill-local-variable 'indent-line-function)
            (set (make-local-variable 'indent-line-function)
                                  'indent-relative)))
(add-to-list 'completion-ignored-extensions ".hi")

;;; Line numbering
;;; (from http://www.emacswiki.org/LineNumbers)
(defvar my-linum-format-string "%4d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "%" (number-to-string width) "d ")))
    (setq my-linum-format-string format)))
(setq linum-format 'my-linum-format)
(defun my-linum-format (line-number)
     (propertize (format my-linum-format-string line-number) 'face 'linum))
(global-linum-mode 1)

;;; Projectile everywhere
(require 'projectile)
(projectile-global-mode)
