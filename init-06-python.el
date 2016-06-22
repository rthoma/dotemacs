;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-06-python.el
;; Emacs, Version 24.5
;; Windows 10 Pro, Version 1511
;; Last edited: June 21, 2016
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
 
(use-package company-quickhelp 
  :ensure t
  :pin melpa-stable
  :defer t 
  :init
  (progn
    (setq company-quickhelp-delay 0.1)
    (use-package pos-tip
      :ensure t
      :pin melpa-stable
      :defer t)))

(use-package yasnippet
  :ensure t 
  :pin gnu
  :defer t 
  :bind ("C-c k" . yas-expand))

(use-package python
  ;;
  ;; Package preferences
  ;;
  :defer t
  ;;
  ;; Preload initialization
  ;;
  :init
  (progn
    (use-package elpy
      :ensure t 
      :pin melpa-stable
      :defer t
      :init
      (setq elpy-rpc-backend "jedi")
      :config 
      (elpy-use-ipython)))
  ;;
  ;; After load configuration
  ;;
  :config
  (progn
    (elpy-enable)
  
    (when (fboundp 'company-quickhelp-mode) (company-quickhelp-mode 1))

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

;; eof
