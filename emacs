;;; package --- summary
(require 'package)

;;; commentary:
;; main Emacs configuration

;;; code:
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  ;; (setq use-package-verbose t)
)

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))
(use-package magit
  :ensure t
  :defer t)
(use-package avy
  :ensure t
  :defer t)
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (
  	(terraform-mode . lsp-deferred)
    (go-mode . lsp-deferred)
    (csharp-mode . lsp-deferred)
    (c-mode . lsp-deferred))
  :config
  (lsp-enable-which-key-integration t))
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :defer t
  :custom
   (lsp-ui-doc-show-with-cursor 't)
   (lsp-ui-doc-delay 1)
   (lsp-ui-doc-position 'at-point)
   (lsp-ui-imenu-auto-refresh 0))
(use-package lsp-ivy
  :ensure t
  :defer t)
(use-package company
  :ensure t
  :defer t)
(use-package yasnippet
  :ensure t
  :defer t)
(use-package yaml-mode
  :ensure t
  :defer t)
(use-package json-mode
  :ensure t
  :defer t)
(use-package ligature :ensure t)
(use-package rg
  :ensure t
  :defer t)
(use-package go-mode
  :ensure t
  :defer t)
(use-package gotest
  :ensure t
  :after go-mode)
(use-package counsel
  :ensure t
  :bind (("C-M-j" . 'counsel-switch-buffer-other-window)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :defer t)
(use-package counsel-projectile
  :ensure t
  :after counsel)
(use-package restclient
  :ensure t
  :defer t)
(use-package uuidgen
  :ensure t
  :defer t)
(use-package exec-path-from-shell
  :ensure t
  :config
 (dolist (var '("GOPATH"  "GOROOT"))
   (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
(use-package vs-dark-theme
  :ensure t
  :config
  (set-frame-font "Fira Code-12")
  (load-theme 'vs-dark t))
(use-package projectile
  :ensure t
  :defer t	
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))
(use-package multiple-cursors
  :ensure t
  :defer t)
(use-package move-text
  :ensure t
  :defer t)
(use-package dockerfile-mode
  :ensure t
  :defer t)
(use-package markdown-mode
  :ensure t
  :defer t)
(use-package go-tag
  :ensure t
  :after go-mode)
(use-package dap-mode
  :ensure t
  :after go-mode	
  :config
  (dap-ui-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (dap-tooltip-mode 1))
(use-package eterm-256color
  :ensure t
  :hook (term-mode . eterm-256color-mode))
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))
(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell))
(use-package eshell-git-prompt
  :ensure t
  :commands eshell	
  :config
  (eshell-git-prompt-use-theme 'git-radar))
(use-package nerd-icons :ensure t)
(use-package mood-line
  :ensure t
  :init (mood-line-mode)
  :config
  (set-face-attribute 'mode-line nil
                      :background "#2E2E2E"
                      :foreground "white"
                      :box '(:line-width 2 :color "#2A2A2A")
                      :overline nil
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#2D2D2D"
                      :foreground "white"
                      :box '(:line-width 2 :color "#2A2A2A")
                      :overline nil
                      :underline nil))
(use-package ivy
  :ensure t
  :defer t)
(use-package ivy-rich
  :ensure t
  :defer t
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-project-root-cache-mode 1))
(use-package terraform-mode
  :ensure t
  :defer t)
(use-package treemacs
  :ensure t
  :defer t)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp-mode))
  
(require 'dired-x)
(require 'dap-dlv-go)

(defun efs/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (add-hook 'eshell-mode-hook
      (lambda ()
              (define-key eshell-mode-map (kbd "C-r") #'counsel-esh-history)))


  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

(defun lsp-csharp-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'csharp-mode-hook #'lsp-csharp-install-save-hooks)

;; other configs
(ligature-set-ligatures 't '("www" "go-mode"))
(ligature-set-ligatures 'prog-mode
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
   ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
   "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
   "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
   "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
   "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
   "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
   "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
   "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
   "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c 0") 'avy-goto-char)
(global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-c r") 'comment-region)
(global-set-key (kbd "C-c j") 'json-pretty-print-buffer)
(global-set-key (kbd "C-c f") 'counsel-git-grep)
(global-set-key (kbd "C-c +") 'enlarge-window)
(global-set-key (kbd "C-c -") 'shrink-window)
(global-set-key (kbd "C-c y") 'company-yasnippet)
(global-set-key (kbd "C-c C-r") 'lsp-rename)
(global-set-key (kbd "C-c C-k") 'treemacs)
(global-set-key (kbd "C-c C-l") 'treemacs-quit)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "M--") 'undo)
(global-set-key (kbd "C-_") 'undo-redo)
(global-set-key (kbd "M-_") 'undo-redo)
(global-set-key (kbd "C-.") 'company-complete)
(global-set-key (kbd "C-/") 'copilot-complete)
(global-set-key (kbd "C-c d") 'lsp-ui-doc-show)
(global-set-key (kbd "C-c h") 'lsp-ui-doc-hide)
(global-set-key (kbd "C-c m") 'lsp-ui-imenu)
(global-set-key (kbd "C-c .") 'lsp-ui-peek-find-references)
(global-set-key (kbd "C-c o") 'lsp-ivy-workspace-symbol)
(global-set-key (kbd "M-/") 'lsp-goto-implementation)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x p") (lambda ()
                                (interactive)
                                (other-window -1)))
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "C-u") 'compile)
(global-set-key (kbd "C-c C-u") 'shell-command)
(global-set-key (kbd "C-c t") 'go-test-current-test)
(global-set-key (kbd "C-c C-t") 'go-test-current-file)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-,") 'duplicate-line)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "C-c C-c C-d") 'dap-debug)
(global-set-key (kbd "C-c C-c C-n") 'dap-next)
(global-set-key (kbd "C-c C-c C-s") 'dap-step-in)
(global-set-key (kbd "C-c C-c C-o") 'dap-step-out)
(global-set-key (kbd "C-c C-c C-k") 'dap-disconnect)
(global-set-key (kbd "C-<next>") 'flycheck-next-error)
(global-set-key (kbd "C-<prior>") 'flycheck-previous-error)

(tool-bar-mode 0)
(menu-bar-mode 0)
(line-number-mode 0)
(global-hl-line-mode 1)
(global-visual-line-mode 1)
(yas-global-mode 1)
(scroll-bar-mode 0)
(put 'erase-buffer 'disabled nil)
(global-ligature-mode 't)
(counsel-projectile-mode)

(setq-default tab-width 4)
(setq user-mail-address "diego.stamigni@gmail.com")
(setq user-full-name "Diego Stamigni")
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq projectile-project-search-path '("~/Developer"))
(setq package-enable-at-startup nil)
(setq magit-repository-directories
        '(("~/Developer" . 0)
          ("~/Developer" . 1)))
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dap-auto-configure-features '(sessions locals controls tooltip))

(add-to-list 'image-types 'svg)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  )

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(package-selected-packages
   '(uuidgen company-restclient restclient rg counsel-projectile projectile lsp-ivy ligature modus-themes zenburn-theme yasnippet-snippets yaml-mode web-mode vs-dark-theme use-package-hydra spacemacs-theme solarized-theme prettier-js powerline neotree magit lsp-ui json-mode jetbrains-darcula-theme hydra go-snippets go-mode go-imports gh-md flycheck-golangci-lint fira-code-mode exec-path-from-shell evil csharp-mode company-quickhelp cmake-mode avy add-node-modules-path)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
