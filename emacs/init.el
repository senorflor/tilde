;;; Slather with elisp
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-archive-enable-alist '(("melpa" deft magit)))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                   ;; General
                      dash
                      exec-path-from-shell
                      flycheck
                      ido-ubiquitous
                      magit
                      org
                      projectile
                      rainbow-delimiters
                      smex
                      tagedit ;; sexp-style html editing
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
                      clojure-mode-extra-font-locking
                      clojurescript-mode
                      paredit

                   ;; Go
                      go-mode

                   ;; Haskell
                      haskell-mode

                   ;; Rust
                      flycheck-rust
                      rust-mode

                   ;; Al Gore
                      flow-minor-mode
                      web-mode
                      prettier-js
                      tide ; Typescript
                      company ; optional tide dep

                   ;; infrastructure
                      terraform-mode

                   ;; Elm
                      elm-mode)
  "Packages required at launchtime.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Global setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unset these modes if they exist
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Ensure quiet startup
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Fix MacOS path
(when window-system
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))



;; Make window title more useful when windowed
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Don't litter filesystem with backups
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Save last visited point in files
(if (> emacs-major-version 24) (save-place-mode 1))
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; Allow us to yes or no ?s with a single letter
(defalias 'yes-or-no-p 'y-or-n-p)

;; Improve echo area interactions
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

;; Allow us to type over selected region
(delete-selection-mode t)

;; Allow us to downcase region without warning
(put 'downcase-region 'disabled nil)

;; Tab-width 2 spaces by default
(setq-default tab-width 2
	      standard-indent 2
	      indent-tabs-mode nil)

;; Enable flycheck globally for syntax checking
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Move around windows spatially
(global-set-key (kbd "C-x C-<up>") 'windmove-up)
(global-set-key (kbd "C-x C-<down>") 'windmove-down)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)

;; Use more interactive/freq-based M-x command completion
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; OS X copy to clipboard on C-w
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

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

;;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
;; Clojurescript/EDN highlighting
(setq auto-mode-alist (cons '("\\.edn$" . clojure-mode) auto-mode-alist))  ; *.edn are Clojure files
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))
;;; cider config
(require 'ac-cider)
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;;; Rust
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; Al Gore
(require 'web-mode)
(require 'flycheck)
(require 'prettier-js)
(require 'flow-minor-mode)
;; From https://github.com/prettier/prettier-emacs
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))
(defun configure-web-mode-flycheck-checkers ()
  ;; in order to have flycheck enabled in web-mode, add an entry to this
  ;; cond that matches the web-mode engine/content-type/etc and returns the
  ;; appropriate checker.
  (-when-let (checker (cond
                       ((string= web-mode-content-type "jsx")
                        'javascript-eslint)))
    (flycheck-mode)
    (flycheck-select-checker checker)))
(defun customize-web-mode-formatting ()
  "Web mode customizer"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (if (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx")))
(add-to-list 'auto-mode-alist '("\\.m?jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq web-mode-engines-alist
      '(("javascript" . "\\.mjs\\'")))
(add-hook 'web-mode-hook 'customize-web-mode-formatting)
(add-hook 'web-mode-hook 'flow-minor-enable-automatically)
(add-hook 'web-mode-hook 'configure-web-mode-flycheck-checkers)

;;; Tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (flow-minor-mode projectile elm-mode web-mode haskell-mode go-mode paredit clojurescript-mode ac-cider groovy-mode yaml-mode markdown-mode undo-tree rainbow-delimiters dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
