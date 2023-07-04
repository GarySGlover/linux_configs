;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                                ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
        (package-install 'use-package))

;; (require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
    :custom
    (auto-package-update-interval 1)
    (auto-package-prompt-before-update t)
    (auto-package-update-hide-results t)
    :config
    (auto-package-update-maybe)
    (auto-package-update-at-time "10:00"))

(require 'server)
(unless (server-running-p)
        (server-start))

(setq disabled-command-function nil)

(setq inhibit-startup-message t)

; (scroll-bar-mode -1)                    ; Disable visible scrollbar
; (tool-bar-mode -1)                      ; Disable the toolbar
; (tooltip-mode -1)                       ; Disable tooltips
(set-fringe-mode 10)                    ; Give some breathing room
; (menu-bar-mode -1)                      ; Disable the menu bar

; (global-unset-key (kbd "C-x C-c"))

(setq vc-follow-symlinks t)

(setq use-short-answers t)

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil
        :font "Fira Code Retina"
        :height 100)

(use-package
        all-the-icons)
;; :init
;; (all-the-icons-install-fonts t)

(use-package
        doom-modeline
        :ensure t
        :init (doom-modeline-mode 1)
        :custom ((doom-modeline-height 15)))

(use-package
        doom-themes
        :init (load-theme 'doom-dracula t))

(setq backup-directory-alist '((".*" . "~/.emacs.d/emacs_backup")))
(setq create-lockfiles nil)
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/emacs_backup" t)))

(defun efs/configure-eshell () P
        (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
        (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
        (setq eshell-history-size 10000 eshell-buffer-maximum-lines 10000 eshell-hist-ignoredups t
                eshell-scroll-to-bottom-on-input t)
        (setq eshell-visual-commands (append eshell-visual-commands '("bash" "zsh" "bluetuith"))))

(use-package
        eshell-git-prompt)

(use-package
        eshell
        :hook (eshell-first-time-mode . efs/configure-eshell)
        :config (with-eval-after-load 'esh-opt
                        (setq eshell-destroy-buffer-when-process-dies t))
        (eshell-git-prompt-use-theme 'powerline))

(define-minor-mode prog-minor-mode
    "Minor mode for programming support.
Should be used as the main mode to hook other modes onto")
(add-hook 'prog-mode-hook #'prog-minor-mode)

(global-auto-revert-mode 1)

(use-package hydra)

(use-package general)

(use-package nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(use-package whole-line-or-region)

(use-package restclient
    :mode ("\\.rest\\'" . restclient-mode))

(use-package keyfreq
    :init
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)
    (setq keyfreq-excluded-commands
        '(self-insert-command
             mouse-drag-region
             org-delete-backward-char
             mouse-set-region
             ivy-backward-delete-char
             mouse-set-point
             ignore
             y-or-n-p-insert-y
             )))

(use-package vlf)

(use-package
        which-key
        :init (which-key-mode)
        :diminish which-key-mode
        :config)

(use-package
        helpful
        :custom (counsel-describe-function-function #'helpful-callable)
        (counsel-describe-variable-function #'helpful-variable)
        :bind ([remap describe-function] . counsel-describe-function)
        ([remap describe-command] . helpful-command)
        ([remap describe-variable] . counsel-describe-variable)
        ([remap describe-key] . helpful-key))

(use-package
        keycast
        :init (keycast-tab-bar-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default lisp-indent-offset 4)

(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

(defun org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
              (expand-file-name "~/linux_configs/emacs_config.org"))
        (let ((org-config-babel-evaluate nil))
            (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda ()
                             (add-hook 'after-save-hook #'org-babel-tangle-config)))

(use-package
        org-bullets
        :after org
        :hook (org-mode . org-bullets-mode))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("bash" . "src bash"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("nims" . "src nims"))

(setq org-src-window-setup 'current-window)

(defun org-babel-remove-result-all ()
    (interactive)
    (org-babel-remove-result-one-or-many 1))
(use-package org-bullets
    :bind ("C-c C-v C-k" . org-babel-remove-result-all))

(use-package
        rainbow-delimiters
        :hook (prog-minor-mode . rainbow-delimiters-mode))

(setq-default fill-column 80)
(add-hook 'prog-minor-mode-hook #'display-fill-column-indicator-mode)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook treemacs-mode-hook eshell-mode-hook))
        (add-hook mode (lambda ()
                               (display-line-numbers-mode 0))))

(use-package
        evil-nerd-commenter
        :bind ("C-;" . evilnc-comment-or-uncomment-lines))

(use-package highlight-indent-guides
  :hook (prog-minor-mode . highlight-indent-guides-mode)
  :bind ("C-c h" . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character))

(use-package dtrt-indent
    :hook (prog-minonr-mode . dtrt-indent-mode)
    :bind (("C-c i d" . dtrt-indent-diagnosis)
            ("C-c i h" . dtrt-indent-highlight)
            ("C-c i u" . dtrt-indent-undo)))

(use-package whitespace
    :hook (prog-minor-mode . whitespace-mode)
    :init (setq whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))
    :bind (("C-c w" . whitespace-mode)))

(use-package ws-butler
    :hook ((prog-minor-mode org-mode) . ws-butler-mode))

; if you're windened, narrow to the region, if you're narrowed, widen
; bound to C-x n
(defun narrow-or-widen-dwim (p)
    "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and
               (buffer-narrowed-p)
               (not p))
              (widen))
        ((region-active-p)
            (narrow-to-region (region-beginning)
                (region-end)))
        ((derived-mode-p 'org-mode)
            ;; `org-edit-src-code' is not a real narrowing command.
            ;; Remove this first conditional if you don't want it.
            (cond ((ignore-errors (org-edit-src-code))
                      (delete-other-windows))
                ((org-at-block-p)
                    (org-narrow-to-block))
                (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))
(general-define-key "C-c n" 'narrow-or-widen-dwim)

(use-package smart-shift
    :init (global-smart-shift-mode 1))

(use-package iedit
    :bind ("C-c e" . 'iedit-mode))

(defhydra hydra-windows (global-map "C-c w" :hint nil)
        ("i" (clover/font-size-increase 5))
        ("d" (clover/font-size-decrease 5))
        ("<up>" windmove-up)
        ("<down>" windmove-down)
        ("<left>" windmove-left)
        ("<right>" windmove-right))
(general-define-key "M-o" 'other-window)

(use-package resize-window
  :bind (("C-c r" . resize-window)))

(defun display-buffer-from-compilation-p (_buffer-name _action)
  (unless current-prefix-arg (with-current-buffer (window-buffer)
                               (derived-mode-p 'compilation-mode))))

(push '(display-buffer-from-compilation-p display-buffer-same-window (inhibit-same-window . nil))
  display-buffer-alist)

(use-package
        counsel
        :bind (("M-x" . counsel-M-x)
                      ("C-x b" . counsel-switch-buffer)
                      ("C-x C-f" . counsel-find-file)
                      :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))
        :custom (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
        :config (setq ivy-initial-inputs-alist nil)
        (setq counsel-switch-buffer-preview-virtual-buffers nil))

(use-package
        swiper
        :ensure t)

(use-package
        ivy

        :diminish
        :bind (("C-s" . swiper) :map ivy-minibuffer-map ("TAB" . ivy-alt-done)
                      ("C-l" . ivy-alt-done)
                      ("C-j" . ivy-next-line)
                      ("C-k" . ivy-previous-line)
                      :map ivy-switch-buffer-map ("C-k" . ivy-previous-line)
                      ("C-l" . ivy-done)
                      ("C-d" . ivy-switch-buffer-kill)
                      :map ivy-reverse-i-search-map ("C-k" . ivy-previous-line)
                      ("C-d" . ivy-reverse-i-search-kill))
        :config (ivy-mode 1))

(use-package
        ivy-rich
        :init (ivy-rich-mode 1))

(use-package avy
    :bind (("C-c s s" . avy-goto-char)
              ("C-c s c" . avy-goto-char-2)
              ("C-c s l" . avy-goto-line)
              ("C-c s w" . avy-goto-word-1))
    )

(use-package
        treemacs)
(general-define-key "C-c d" 'treemacs-select-window)

(use-package
        projectile
        :diminish projectile-mode
        :config (projectile-mode)
        :bind-keymap ("C-c p" . projectile-command-map)
        :init (setq projectile-switch-project-action #'projectile-dired))

;; Projectile Counsel
(use-package
        counsel-projectile
        :after projectile
        :config (counsel-projectile-mode 1))

(use-package
        magit
        :commands (magit-status magit-get-current-branch)
        :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
        :bind ("C-c g" . magit-status))

(defun efs/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))

(use-package
    lsp-mode
    :commands (lsp lsp-deferred)
    :init (setq lsp-keymap-prefix "C-c l")
    :config (lsp-enable-which-key-integration t)
    :hook (lsp-mode . efs/lsp-mode-setup)
    (prog-mode . lsp-deferred))

(use-package
        lsp-ui
        :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package
        company
        :after lsp-mode
        :hook (prog-minor-mode . company-mode)
        :bind (:map company-active-map
                      ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
                ("<tab>" . company-indent-or-complete-common))
        :custom (company-minimum-prefix-length 1)
        (company-idle-delay 0.0))

(use-package
        company-box
        :hook (company-mode . company-box-mode))

(use-package
        elisp-format
        :bind (:map emacs-lisp-mode-map
                      ("C-c f" . elisp-format-buffer)))

(use-package
    ob-powershell)
(use-package
    powershell
    :init (add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))
    :hook
    (powershell-mode . (lambda ()
                           (set (make-local-variable 'compile-command)
                               (format "pwsh -NoLogo -NonInteractive -Command \"& '%s'\""
                                   (buffer-file-name))))))

(use-package
    jq-mode
    :init    (add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode)))

(use-package
    terraform-mode
    :hook (terraform-mode . prog-minor-mode))

(use-package
    yaml-mode
    :init (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
    :hook (yaml-mode . prog-minor-mode))

(setq exec-path (append exec-path '("~/.nimble/bin")))
(use-package
    nim-mode
    :hook (nim-mode . prog-minor-mode))

(use-package pyvenv)
(use-package pipenv)
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(defun clover/set-frame-font-size (SIZE)
        (interactive "nFont Size: ")
        (set-face-attribute 'default (selected-frame)
                :height SIZE))

(defun clover/font-size-increase (BY)
        (interactive "nFont Size Increase Amount: ")
        (let ((height (face-attribute 'default
                              :height (selected-frame))))
                (clover/set-frame-font-size (+ BY height))))

(defun clover/font-size-decrease (BY)
        (interactive "nFont Size Decrease Amount: ")
        (clover/font-size-increase (- BY)))

(defun clover-counsel-switch-buffer (regex-list)
        (let ((ivy-ignore-buffers (append ivy-ignore-buffers regex-list)))
                (ivy-switch-buffer)))

(defun clover-show-only-firefox-buffers ()
        (interactive)
        (clover-ignore-star-and-buffers '("^[^F][^i][^r]")))

(defun clover-show-only-brave-buffers ()
        (interactive)
        (clover-ignore-star-and-buffers '("^[^B][^r][^a][^v][^e]")))

(defun clover-ignore-star-buffers ()
        "ignore everything starting with a star along with whatever ivy's defaults are"
        (interactive)
        (clover-counsel-switch-buffer (append ivy-ignore-buffers '("^\*"))))

(defun clover-ignore-star-and-buffers (regex-list)
        (interactive)
        (clover-counsel-switch-buffer (append ivy-ignore-buffers '("^\*") regex-list)))

(general-define-key "C-x b" 'clover-ignore-star-buffers)
