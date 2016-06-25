;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-06-python.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python setup
;;
(use-package iedit
  :ensure t
  :pin melpa-stable
  :defer t
  :bind ("C-c o" . iedit-mode))

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
        (elpy-use-ipython)
        (elpy-enable)))

    (use-package yasnippet
      :ensure t
      :pin gnu
      :defer t
      :bind ("C-c k" . yas-expand))

    (use-package company-quickhelp
      :ensure t
      :pin melpa-stable
      :defer t
      :init
      (setq company-quickhelp-delay 0.1))

    (use-package pos-tip
      :ensure t
      :pin melpa-stable
      :defer t)

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
         company-active-map)))

    (when (fboundp 'company-quickhelp-mode) (company-quickhelp-mode 1))))

;; eof
