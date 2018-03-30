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
  :bind ("C-c i" . iedit-mode))

(use-package yasnippet
  :ensure t
  :pin gnu
  :defer t
  :config
  (bind-keys :map yas-minor-mode-map
             ("C-c e" . yas-expand)))

(use-package company
  ;;
  ;; Package preferences
  ;;
  :ensure t
  :pin gnu
  ;;
  ;; Pre-load initialization
  ;;
  :defer t
  :init
  (setq company-idle-delay 0.1)
  ;;
  ;; After load configuration
  ;;
  :config
  (progn
    (bind-keys :map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous))

    (use-package company-quickhelp
      :ensure t
      :pin melpa-stable
      :defer 2
      :init
      (setq company-quickhelp-delay 0.1)
      :config
      (when (fboundp 'company-quickhelp-mode) (company-quickhelp-mode 1)))

    (defun rthoma/company-yasnippet-or-completion ()
      "Solve company yasnippet conflicts."
      (interactive)
      (let ((yas-fallback-behavior
            (apply #'rthoma/company-complete-common nil)))
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
  ;; Pre-load initialization
  ;;
  :defer t
  ;;:init
  ;;
  ;; After load configuration
  ;;
  :config
  (use-package elpy
    :ensure t
    :pin melpa-stable
    :init
    (setq elpy-rpc-backend "jedi")
    :config
    (progn
      (elpy-use-ipython)
      (elpy-enable))))

;; eof
