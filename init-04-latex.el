;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-04-latex.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spell-checker setup
;;
;; Add aspell brew directory to path on macOS
(when (eq system-type 'darwin)
      (setenv "PATH" (concat (getenv "PATH")
              ":/usr/local/Cellar/aspell/0.60.6.1_1/bin"))
      (setq exec-path (append exec-path
             '("/usr/local/Cellar/aspell/0.60.6.1_1/bin")))

      ;; Settings for aspell
      (setq ispell-program-name "aspell"
            ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

;; Tell Windows where to find hunspell
(when (eq system-type 'windows-nt)
      (setq ispell-program-name "C:/msys64/mingw64/bin/hunspell")
      (setq ispell-local-dictionary "en_US")
      (setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LaTeX setup
;;
(use-package latex
  ;;
  ;; Package preferences
  ;;
  :ensure auctex
  :pin melpa
  :defer t
  ;;
  :bind (("C-c o" . fill-region)
         :map LaTeX-mode-map)
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
    (when (eq system-type 'darwin)
      (setq TeX-view-program-list
       '(("Preview" "open -a Preview.app %o")
         ("Skim" "open -a Skim.app %o")
         ("displayline"
          "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
         ("open" "open %o")))

      ;; Select the viewers for each file type
      (setq TeX-view-program-selection
       '((output-dvi "open")
         (output-pdf "displayline")
         (output-html "open"))))

    ;; Set the list of viewers for Windows
    (when (eq system-type 'windows-nt)
      (setq TeX-view-program-list
       '(("Sumatra"
          "\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance %o")
         ("displayline"
          "\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance -forward-search %b %n %o")
         ("open" "open %o")))

      ;; Select the viewers for each file type
      (setq TeX-view-program-selection
       '((output-dvi "open")
         (output-pdf "displayline")
         (output-html "open"))))

    ;; Select command latexmk
    (setq TeX-command-default "latexmk")
    (setq reftex-plug-into-AUCTeX t)
    (setq bibtex-align-at-equal-sign t)

    (setq LaTeX-indent-environment-list
         '(("frame")
           ("verbatim" current-indentation)
           ("verbatim*" current-indentation)
           ("tabular")     ;;  LaTeX-indent-tabular
           ("tabular*")    ;;  LaTeX-indent-tabular
           ("align")       ;;  LaTeX-indent-tabular
           ("align*")      ;;  LaTeX-indent-tabular
           ("array")       ;;  LaTeX-indent-tabular
           ("eqnarray")    ;;  LaTeX-indent-tabular
           ("eqnarray*")   ;;  LaTeX-indent-tabular
           ("displaymath")
           ("equation")
           ("equation*")
           ("picture")
           ("tabbing")))

    (setq LaTeX-indent-level 4)
    (setq LaTeX-item-indent 0)
    (setq LaTeX-left-right-indent-level 4)
    (setq TeX-brace-indent-level 4)
    (setq tex-indent-item 4)
    (setq tex-indent-basic 4)
    (setq tex-indent-arg 4)
    (setq reftex-level-indent 4)

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
    (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk" t)))

;; eof
