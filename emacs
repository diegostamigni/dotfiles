;;; package --- summary
(require 'package)

;;; commentary:
;; main Emacs configuration

;;; code:
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
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
  (require 'use-package))

(setenv "path" (concat (getenv "path") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setenv "path" (concat (getenv "path") ":/usr/local/share/dotnet"))
(setq exec-path (append exec-path '("/usr/local/share/dotnet")))

(setenv "path" (concat (getenv "path") ":/opt/homebrew/bin/"))
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

(setenv "path" (concat (getenv "path") ":~/Developer/go/bin/"))
(setq exec-path (append exec-path '("~/Developer/go/bin")))

(setenv "path" (concat (getenv "path")":~/.deno/bin"))
(setq exec-path (append exec-path '("~/.deno/bin")))

(use-package magit :ensure t)
(use-package avy :ensure t)
(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t)
(use-package company :ensure t)
(use-package company-restclient :ensure t)
(use-package yasnippet :ensure t)
(use-package csharp-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package json-mode :ensure t)
(use-package neotree :ensure t)
(use-package ligature :ensure t)
(use-package lsp-ivy :ensure t)
(use-package rg :ensure t)
(use-package go-mode :ensure t)
(use-package counsel-projectile :ensure t)
(use-package restclient :ensure t)
(use-package uuidgen :ensure t)
(use-package exec-path-from-shell
  :ensure t
  :defer  2
  :config
  (dolist (var '("GOPATH"  "GOROOT"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
(use-package lsp-mode :ensure t)
;;(use-package zenburn-theme
;;(use-package jetbrains-darcula-theme
(use-package vs-dark-theme
  :ensure t
  :config
  (set-frame-font "Fira Code-12")
  (load-theme 'vs-dark t))
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
(use-package dockerfile-mode :ensure t)
(use-package docker-compose-mode :ensure t)

(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

(add-hook 'csharp-mode-hook #'lsp-deferred)
(defun lsp-csharp-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'csharp-mode-hook #'lsp-csharp-install-save-hooks)

;; copilot
;; (add-hook 'prog-mode-hook 'copilot-mode)
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))
  
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

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

(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c 0") 'avy-goto-char)
(global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-c r") 'comment-region)
(global-set-key (kbd "C-c j") 'json-pretty-print-buffer)
(global-set-key (kbd "C-c f") 'vc-git-grep)
(global-set-key (kbd "C-c +") 'enlarge-window)
(global-set-key (kbd "C-c -") 'shrink-window)
(global-set-key (kbd "C-c y") 'company-yasnippet)
(global-set-key (kbd "C-c C-r") 'lsp-rename)
(global-set-key (kbd "C-c C-k") 'neotree-show)
(global-set-key (kbd "C-c C-l") 'neotree-hide)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "M--") 'undo)
(global-set-key (kbd "C-_") 'undo-redo)
(global-set-key (kbd "M-_") 'undo-redo)
(global-set-key (kbd "C-.") 'company-complete)
(global-set-key (kbd "C-/") 'copilot-complete)
(global-set-key (kbd "C-c d") 'lsp-ui-doc-show)
(global-set-key (kbd "C-c m") 'lsp-ui-imenu)
(global-set-key (kbd "C-c .") 'lsp-ui-peek-find-references)
(global-set-key (kbd "C-c o") 'lsp-ivy-workspace-symbol)
(global-set-key (kbd "M-/") 'lsp-goto-implementation)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-c d") 'docker)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x p") (lambda ()
                                (interactive)
                                (other-window -1)))

(tool-bar-mode 0)
(menu-bar-mode 0)
(linum-mode 0)
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
(setq lsp-ui-imenu-auto-refresh 0)
(setq counsel-mode t)
(setq projectile-project-search-path '("~/Developer" ))
(setq package-enable-at-startup nil)
(add-to-list 'image-types 'svg)

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

;; adds company-restclient to company for auto-completion
(add-to-list 'company-backends 'company-restclient)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
