;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-06-python.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;; Windows 10 Pro, Version 1511, Build 10586.420
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Auxiliary packages frequently used with Python
;;
(use-package iedit
  :ensure t
  :pin melpa-stable
  :defer t
  :bind ("C-c i" . iedit-mode))

(use-package yasnippet
  :ensure t
  :pin gnu
  :defer t
  :config
  (define-key yas-minor-mode-map (kbd "C-c k") #'yas-expand))

(use-package company
  ;;
  ;; Package preferences
  ;;
  :ensure t
  :pin gnu
  :defer t
  ;;
  ;; After load configuration
  ;;
  :config
  (progn
    (use-package company-quickhelp
      :ensure t
      :pin melpa-stable
      :defer t
      :init
      (setq company-quickhelp-delay 0.1))

    (defun company-yasnippet-or-completion ()
      "Solve company yasnippet conflicts."
      (interactive)
      (let ((yas-fallback-behavior
            (apply 'company-complete-common nil)))
        (yas-expand)))

    (add-hook 'company-mode-hook
      (lambda ()
        (substitute-key-definition
        'company-complete-common
        'company-yasnippet-or-completion
         company-active-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python setup
;;
(use-package python
  ;;
  ;; Package preferences
  ;;
  :defer t
  ;;
  ;; Preload initialization
  ;;
  ;;:init
  ;;
  ;; After load configuration
  ;;
  :config
  (progn
    (use-package elpy
      :ensure t
      :pin melpa-stable
      :init
      (setq elpy-rpc-backend "jedi")
      :config
      (progn
        (setq python-shell-interpreter "ipython"
              python-shell-interpreter-args "-i --simple-prompt")
        (elpy-enable)))

    (when (fboundp 'company-quickhelp-mode) (company-quickhelp-mode 1))))

;; eof
