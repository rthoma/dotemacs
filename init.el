;;; -*- lexical-binding: t -*-

(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))

(add-hook 'after-init-hook #'garbage-collect t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

(eval-and-compile
  (defconst emacs-start-time (current-time))

  (defun report-time-since-load (&optional suffix)
    (message "Loading init...done (%.3fs)%s"
             (float-time (time-subtract (current-time) emacs-start-time))
             suffix)))

(add-hook 'after-init-hook
          #'(lambda () (report-time-since-load " [after-init]"))
          t)

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
          #'(lambda ()
              (setq file-name-handler-alist file-name-handler-alist-old)))

(remove-hook 'after-change-major-mode-hook
             'global-eldoc-mode-enable-in-buffers)

(global-eldoc-mode -1)

(require 'package)

(setq package-enable-at-startup nil
      package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(when (eq system-type 'windows-nt)
  (setq package-check-signature nil))

;; only refresh archives if no metadata and no package directory
(unless (or package-archive-contents
            (file-exists-p package-user-dir))
  (package-refresh-contents))

;; install use-package if missing
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package))

(let ((verbose (or nil init-file-debug)))
  (setq use-package-verbose verbose
        use-package-expand-minimally (not verbose)
        use-package-compute-statistics verbose
        debug-on-error verbose
        debug-on-message "finalizer failed"
        debug-on-quit verbose))

(use-package emacs                      ;; not an actual package
  :no-require t
  ;;
  :custom
  (inhibit-startup-screen t)
  (line-number-mode t)
  (column-number-mode t)

  ;; bytecomp.el
  (byte-compile-verbose nil)

  ;; files.el
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 3)
  (kept-old-versions 2)
  (version-control t)

  ;; paragraphs.el
  (sentence-end-double-space nil)

  ;; paren.el
  (show-paren-delay 0)

  ;; time.el
  (display-time-24hr-format t)
  (display-time-default-load-average nil)
  ;;
  :init
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (display-time)

  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

  (setq-default indent-tabs-mode nil)
  (setq-default octave-block-offset 4)

  (when (eq system-type 'darwin)
    (setq mac-option-key-is-meta t
          mac-command-key-is-meta nil
          mac-command-modifier 'control
          mac-option-modifier 'meta))

  (when (eq system-type 'windows-nt)
    (setq w32-pass-lwindow-to-system nil
          w32-pass-rwindow-to-system nil
          w32-pass-alt-to-system nil))

  (let ((font (cond
               ((eq system-type 'darwin) "Menlo-12")
               ((eq system-type 'windows-nt) "Consolas-10")
               ((eq system-type 'gnu/linux) "DejaVu Sans Mono-12"))))
    (when font
      (add-to-list 'default-frame-alist `(font . ,font))
      (set-frame-font font nil t)))
  ;;
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)

  (defun rthoma/terminal-visible-bell ()
    "A friendlier visual bell effect."
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line))

  (setq visible-bell nil
        ring-bell-function #'rthoma/terminal-visible-bell)

  (let ((backup-dir (expand-file-name "~/.emacs.d/saves")))
    (unless (file-exists-p backup-dir)
      (make-directory backup-dir t))
    (setq backup-directory-alist `(("." . ,backup-dir)))))

(use-package recentf
  :demand t
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  ;;
  :custom
  (recentf-auto-cleanup 60)
  (recentf-exclude
   '("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'"))
  (recentf-max-saved-items 1024)
  ;;
  :preface
  (defun rthoma/recentf-add-dired-directory ()
    "Add directories visit by dired into recentf."
    (if (and dired-directory
             (stringp dired-directory)
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  ;;
  :hook (dired-mode . rthoma/recentf-add-dired-directory)
  ;;
  :config
  (recentf-mode 1))

(use-package savehist
  :unless noninteractive
  ;;
  :custom
  (savehist-additional-variables
   '(file-name-history
     kmacro-ring
     compile-history
     compile-command))
  (savehist-autosave-interval 60)
  (savehist-ignored-variables
   '(load-history
     flyspell-auto-correct-ring
     org-roam-node-history
     magit-revision-history
     org-read-date-history
     query-replace-history
     yes-or-no-p-history
     kill-ring))
  (savehist-mode t)
  ;;
  :config
  (savehist-mode 1))

(use-package server  ;; Start Emacs server (i.e., emacs --daemon)
  :defer 2
  :config
  (when (fboundp 'server-running-p)
    (unless (server-running-p)
      (server-start))))

(use-package whitespace
  :diminish whitespace-mode
  :bind (:map whitespace-mode-map
              ("C-c s w" . whitespace-mode))
  :init
  (setq whitespace-line-column 79)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package completion-preview
  :demand t
  :if (version<= "30.1" emacs-version)
  :diminish completion-preview-mode
  :config
  (global-completion-preview-mode 1))

(use-package consult
  :ensure t
  :bind (("C-s"   . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop))
  :custom
  ;; Preview results immediately on selection
  (consult-preview-key 'any))

(use-package consult-dir
  :ensure t
  :bind (("M-g d" . consult-dir)
         :map minibuffer-local-completion-map
         ("M-s f" . consult-dir-jump-file)
         ("M-g d" . consult-dir)))

(use-package consult-dir-vertico        ;; not an actual package
  :no-require t
  :after (consult-dir vertico)
  :defines (vertico-map)
  :bind (:map vertico-map
              ("M-g d" . consult-dir)
              ("M-s f" . consult-dir-jump-file)))

(use-package corfu
  :ensure t
  :demand t
  :bind (("M-/" . completion-at-point)
         :map corfu-map
         ("C-n"      . corfu-next)
         ("C-p"      . corfu-previous)
         ("<escape>" . corfu-quit)
         ("<return>" . corfu-insert))
  ;;
  :custom
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  ;;
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :demand t
  :bind (:prefix-map
         my-cape-map
         :prefix "C-c ."
         ("p" . completion-at-point)
         ("t" . complete-tag)
         ("d" . cape-dabbrev)
         ("f" . cape-file)
         ("k" . cape-keyword)
         ("s" . cape-elisp-symbol)
         ("a" . cape-abbrev)
         ("l" . cape-line))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package elisp-mode-cape            ;; not an actual package
  :no-require t
  :after (cape elisp-mode)
  :hook (emacs-lisp-mode . rthoma/setup-elisp)
  :preface
  (defun rthoma/setup-elisp ()
    (setq-local completion-at-point-functions
                `(,(cape-capf-super
                    #'elisp-completion-at-point
                    #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 5)))

(use-package embark
  :bind (("M-."   . embark-act)
         ("C-M-." . embark-act-all)
         ("C-h b" . embark-bindings)    ;; alternative for `describe-bindings'
         ("C-c v" . embark-dwim)
         :map embark-collect-mode-map
         ("C-c C-a" . embark-collect-direct-action-minor-mode))
  ;;
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  ;;
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;;
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult             ;; package hosted on elpa
  :ensure t
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :after cape                           ; because this defines C-c .
  :demand t
  ;;
  :custom
  (vertico-count 10)
  (vertico-cycle t)
  ;;
  :config
  (vertico-mode)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Hide commands in M-x which do not work in the current mode. Vertico
  ;; commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (use-package vertico-repeat
    :demand t
    :bind
    (("C-c . ." . vertico-repeat)
     :map vertico-map
     ("M-P" . vertico-repeat-previous)
     ("M-N" . vertico-repeat-next))
    :hook
    (minibuffer-setup . vertico-repeat-save))

  ;; Enable ivy-like path editing
  (use-package vertico-directory
    :ensure nil                         ;; it's included in vertico
    :bind (:map vertico-map
                ("<backspace>"   . vertico-directory-delete-char)
                ("C-w"           . vertico-directory-delete-word)
                ("C-<backspace>" . vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  (use-package vertico-quick
    :demand t
    :bind (:map vertico-map
                ("C-." . vertico-quick-exit)
                ("<S-return>" . vertico-quick-exit)
                ("M->" . vertico-quick-embark))
    :preface
    (defun vertico-quick-embark (&optional arg)
      "Embark on candidate using quick keys."
      (interactive)
      (when (vertico-quick-jump)
        (embark-act arg)))))

(use-package yasnippet
  :ensure t
  :defer t
  ;;
  :diminish yas-minor-mode
  :commands yas-minor-mode-on
  ;;
  :bind (("C-c y x" . yas-expand)
         :map yas-keymap
         ("C-i" . yas-next-field-or-maybe-expand))
  :hook (prog-mode . yas-minor-mode-on)
  ;;
  :custom
  (yas-prompt-functions '(yas-completing-prompt yas-no-prompt))
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  ;;
  :config
  (yas-reload-all)
  ;; Add yasnippet to the completion sources
  (add-to-list 'completion-at-point-functions #'cape-yasnippet))

(use-package yasnippet-org              ;; not an actual package
  :no-require t
  :after org
  ;;
  :hook
  (org-mode . yas-minor-mode-on)
  (org-tab-after-check-for-cycling . yas-expand))

(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet))

(use-package exec-path-from-shell
  :ensure t
  :defer 3
  :config
  (when (eq system-type 'darwin)
    (exec-path-from-shell-initialize)))

(use-package iedit
  :ensure t
  :defer t
  :bind (("C-c i" . iedit-mode)))

(use-package json-mode
  :ensure t
  :defer t
  :bind (:map json-mode-map
              ("C-c C-f" . json-pretty-print-buffer)))

(use-package json-snatcher
  :ensure t
  :defer t
  :after json-mode)

(use-package latex
  :ensure auctex
  :defer t
  ;;
  :bind (:map LaTeX-mode-map
              ("C-c o" . fill-region))
  ;;
  :init
  (when (eq system-type 'darwin)
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
          exec-path (append exec-path
                            '("/usr/texbin" "/usr/local/Cellar/aspell/0.60.8/bin")))

    (setenv "PATH" (concat (getenv "PATH")
                           ":/usr/local/Cellar/aspell/0.60.8/bin:/usr/texbin")))

  (when (eq system-type 'windows-nt)
    (setq ispell-program-name "C:/Program Files/GNU Emacs/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe"
          ispell-local-dictionary "en_US"
          ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
  ;;
  :hook
  (bibtex-mode . (lambda () (set-fill-column 69)))

  ;; Turn on flyspell, math mode, and reftex by default
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)

  (TeX-mode . (lambda ()
                (setq TeX-command-default "latexmk")))
  ;;
  :config
  ;; Set the list of viewers for macOS
  (when (eq system-type 'darwin)
    (setq TeX-view-program-list
          '(("Preview" "open -a Preview.app %o")
            ("Skim" "open -a Skim.app %o")
            ("displayline"
             "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
            ("open" "open %o"))))

  ;; Set the list of viewers for Windows
  (when (eq system-type 'windows-nt)
    (setq TeX-view-program-list
          '(("Sumatra"
             "\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance %o")
            ("displayline"
             "\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance -forward-search %b %n %o")
            ("open" "open %o"))))

  ;; Select the viewer for each file type
  (setq TeX-view-program-selection
        '((output-dvi "open")
          (output-pdf "displayline")
          (output-html "open")))

  ;; Set up LaTeX to use latexmk and make available by C-c C-c
  (unless (assoc "latexmk" TeX-command-list)
    (push '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
            :help "Run latexmk on file")
          TeX-command-list))

  (setq LaTeX-indent-environment-list
        '(("frame")
          ("verbatim" current-indentation)
          ("verbatim*" current-indentation)
          ("tabular")
          ("tabular*")
          ("align")
          ("align*")
          ("array")
          ("eqnarray")
          ("eqnarray*")
          ("displaymath")
          ("equation")
          ("equation*")
          ("picture")
          ("tabbing")))

  (setq reftex-plug-into-AUCTeX t
        bibtex-align-at-equal-sign t
        LaTeX-indent-level 4
        LaTeX-item-indent 0
        LaTeX-left-right-indent-level 4
        TeX-brace-indent-level 4
        tex-indent-item 4
        tex-indent-basic 4
        tex-indent-arg 4
        reftex-level-indent 4)

  ;; Add files with this extension to the clean up list
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk" t))

(use-package markdown-mode
  :ensure t
  :defer t
  ;;
  :commands (markdown-mode gfm-mode)
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  ;;
  :init
  (setq markdown-command "multimarkdown")
  ;;
  :hook
  (markdown-mode . (lambda () (setq markdown-fontify-code-blocks-natively t))))

(use-package octave
  :ensure nil                           ;; built-in
  :defer t
  :mode ("\\.m\\'" . octave-mode))

(use-package org
  :ensure nil                           ;; built-in
  :defer t
  ;;
  :mode ("\\.org\\'" . org-mode)
  ;;
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda)
              ("C-c c" . org-capture)
              ("C-c b" . org-iswitchb))
  ;;
  :config
  (setq org-todo-keywords
        '((sequence "TODO" "PROG" "WAIT" "CANC" "DONE")))

  (setq org-todo-keyword-faces
        '(("TODO" . org-todo)
          ("PROG" . (:foreground "#74D3D5" :weight bold))
          ("WAIT" . (:foreground "#F2E1AC" :weight bold))
          ("CANC" . org-todo)
          ("DONE" . org-done)))

  (setq org-directory "~/.emacs.d/orgfiles/"
        org-archive-location "~/.emacs.d/orgfiles/archive/"
        user-full-name "rthoma"
        org-src-preserve-indentation t
        org-log-done 'time))

(use-package python
  :defer t
  ;;
  :hook
  (python-mode . eglot-ensure)
  (python-mode . yas-minor-mode)
  ;;
  :config
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt"
        python-shell-prompt-detect-failure-warning nil)

  (defun rthoma/python-interrupt ()
    "Send SIGINT to the Python process named 'Python'."
    (interactive)
    (let ((proc (get-process "Python")))
      (if (and proc (process-live-p proc))
          (interrupt-process proc)
        (message "No active Python process found."))))

  (define-key python-mode-map (kbd "C-c C-k") #'rthoma/python-interrupt)
  (define-key inferior-python-mode-map (kbd "C-c C-k") #'rthoma/python-interrupt))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(report-time-since-load)

;; Local Variables:
;; byte-compile-warnings: (not docstrings lexical noruntime)
;; End:

;; eof
