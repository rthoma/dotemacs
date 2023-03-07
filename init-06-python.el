;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-06-python.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Auxiliary packages frequently used with Python
;;
(use-package iedit
  :ensure t
  :pin melpa
  :defer t
  :bind ("C-c i" . iedit-mode))

(use-package yasnippet
  :ensure t
  :pin gnu
  :defer t
  :config
  (define-key yas-minor-mode-map (kbd "C-c k") #'yas-expand))

(use-package company
  :ensure t
  :pin gnu
  :defer t
  :init
  (setq company-idle-delay 0.1)
  :config
  (progn
    (bind-keys :map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous))

    (use-package company-quickhelp
      :ensure t
      :pin melpa
      :defer t
      :init
      (setq company-quickhelp-delay 0.1)
      :config
      (when (fboundp 'company-quickhelp-mode) (company-quickhelp-mode 1)))

    (defun rthoma/company-yasnippet-or-completion ()
      "Solve company yasnippet conflicts."
      (interactive)
      (let ((yas-fallback-behavior
            (apply 'company-complete-common nil)))
        (yas-expand)))

    (add-hook 'company-mode-hook
      (lambda ()
        (substitute-key-definition
          'company-complete-common
         #'rthoma/company-yasnippet-or-completion
           company-active-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python setup
;;
(use-package python
  :defer t
  :config
  (progn
    (use-package elpy
      :ensure t
      :pin melpa
      :init
      (setq elpy-rpc-backend "jedi")
      (setq elpy-shell-use-project-root nil)
      :config
      (progn
        (when (eq system-type 'darwin)
              (setq python-shell-interpreter "ipython3"
                    python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i"))

        (when (eq system-type 'windows-nt)
              (setq python-shell-interpreter "ipython"
                    python-shell-interpreter-args "-i --simple-prompt"))

        (elpy-enable)))

 ;; ;; Python indentation setup
 ;; (defun rthoma/python-indent-config ()
 ;;   "For use in python-mode-hook."
 ;;   (local-set-key (kbd "<tab>")
 ;;     (lambda () (interactive) (rthoma/indent-by-inserting-spaces 4)))
 ;;   (local-set-key (kbd "<backtab>")
 ;;     (lambda () (interactive) (rthoma/unindent-by-removing-spaces 4))))
 ;;
 ;; ;; Add custom indentation to mode hook
 ;; (add-hook 'python-mode-hook #'rthoma/python-indent-config)

    (when (fboundp 'company-quickhelp-mode) (company-quickhelp-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Octave setup
;;
(use-package octave
  :defer t
  :ensure t
  :mode ("\\.m$" . octave-mode)
  :init
  (setq octave-comment-char ?%)
  :config
  (progn
    (setq comment-start "%")
    (setq comment-add 0)

    (add-hook 'octave-mode-hook
      (lambda ()
        (setq-local smie-indent-functions
                    (delete 'octave-indent-comment smie-indent-functions))))))

;; eof
