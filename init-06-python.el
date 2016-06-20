;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-06-python.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;; Last edited: June 17, 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python setup
;;
(use-package python ;; deferred
  ;;
  ;; Package preferences
  ;;
  :ensure elpy
  :pin melpa-stable
  :defer t
  ;;
  ;; Preload initialization
  ;;
  :bind (("C-c k" . yas-expand)
         ("C-c o" . iedit-mode))
  ;;
  :init
  (setq elpy-rpc-backend "jedi")
  (setq company-quickhelp-delay 0.1)
  ;;
  ;; After load configuration
  ;;
  :config
  (progn
    (elpy-enable)
    (elpy-use-ipython)
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
