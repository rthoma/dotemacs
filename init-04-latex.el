;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-04-latex.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;; Windows 10 Pro, Version 1511, Build 10586.420
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spell-checker setup
;;
(cond
  ((eq system-type 'darwin)  ;; Add aspell brew directory to path on macOS
    (setq ispell-program-name "aspell")
    (setenv "PATH" (concat (getenv "PATH")
            ":/usr/local/Cellar/aspell/0.60.6.1/bin"))
    (setq exec-path (append exec-path
           '("/usr/local/Cellar/aspell/0.60.6.1/bin"))))
  ((eq system-type 'windows-nt)  ;; Tell Windows where to find aspell
    (setq ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")))

(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LaTeX setup
;;
(use-package latex
  ;;
  ;; Package preferences
  ;;
  :ensure auctex
  :pin melpa-stable
  :defer t
  ;;
  ;; Pre-load initialization
  ;;
  :init
  (when (eq system-type 'darwin)  ;; Add texbin to path and exec-path
        (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
        (setq exec-path (append exec-path '("/usr/texbin"))))
  ;;
  ;; After load configuration
  ;;
  :config
  (progn
    ;; Set the list of viewers for macOS
    ;; The -b displayline option highlights the current line
    ;; The -g displayline option launches Skim in the background
    (when (eq system-type 'darwin)
      (setq
       TeX-view-program-list
       '(("Preview.app" "open -a Preview.app %o")
         ("Skim" "open -a Skim.app %o")
         ("displayline"
          "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
         ("open" "open %o")))

      ;; Select the viewers for each file type
      (setq TeX-view-program-selection
       '((output-dvi "open")
         (output-pdf "displayline")
         (output-html "open"))))

    (when (eq system-type 'windows-nt)
      (setq
       TeX-view-program-list
       '(("Sumatra PDF" ("\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
         (mode-io-correlate " -forward-search %b %n ") " %o"))))

       '(progn
          (assq-delete-all 'output-pdf TeX-view-program-selection)
          (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

    ;; Select command latexmk
    (setq TeX-command-default "latexmk")
    (setq reftex-plug-into-AUCTeX t)
    (setq bibtex-align-at-equal-sign t)

    ;; LaTeX indentation setup
    (defun rthoma/latex-indent-config ()
      "For use in LaTeX-mode-hook."
      (local-set-key (kbd "<tab>")
        (lambda () (interactive) (rthoma/indent-by-inserting-spaces 4)))
      (local-set-key (kbd "<backtab>")
        (lambda () (interactive) (rthoma/unindent-by-removing-spaces 4))))

    ;; Add custom indentation to mode hook
    (add-hook 'LaTeX-mode-hook #'rthoma/latex-indent-config)
    (add-hook 'bibtex-mode-hook #'rthoma/latex-indent-config)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 88)))

    ;; Set up LaTeX to use latexmk and make available by C-c C-c
    (add-hook 'LaTeX-mode-hook
      (lambda () (push
       '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
          :help "Run latexmk on file")
            TeX-command-list)))
    (add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "latexmk")))

    ;; Turn on flyspell, math mode, and reftex by default
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)

    ;; Add files with this extension to the clean up list
    (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk" t)

    ;; Key bindings
    (bind-keys :map LaTeX-mode-map
               ("C-c o" . fill-region))))

;; eof
