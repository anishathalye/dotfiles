(add-to-list 'load-path "~/.emacs.d/vendor/use-package")
(require 'use-package)

;; this needs to be loaded before evil
(use-package goto-chg
  :load-path "vendor/goto-chg")

(use-package evil
  :load-path "vendor/evil"

  :init
  (setf evil-want-C-u-scroll t)
  (setf evil-want-fine-undo t)
  (setf evil-want-abbrev-expand-on-insert-exit nil)

  :config
  ;; enable evil mode
  (evil-mode 1)

  ;; make bindings more vim-like
  (defun minibuffer-keyboard-quit ()
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setf deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
  (global-set-key (kbd "RET") 'newline-and-indent)

  ;; key bindings
  (evil-ex-define-cmd "!" 'shell-command)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map ";" 'buffer-menu)

  ;; proof mode bindings
  (evil-define-key 'normal proof-mode-map
    (kbd "<right>") 'proof-goto-point
    (kbd "<up>") 'proof-undo-last-successful-command
    (kbd "<down>") 'proof-assert-next-command-interactive
    (kbd "<left>") 'proof-goto-end-of-locked)
  (evil-define-key 'insert proof-mode-map
    (kbd "<right>") 'proof-goto-point
    (kbd "<up>") 'proof-undo-last-successful-command
    (kbd "<down>") 'proof-assert-next-command-interactive
    (kbd "<left>") 'proof-goto-end-of-locked)

  ;; fstar mode bindings
  (evil-define-key 'normal fstar-mode-map
    (kbd "<right>") 'fstar-subp-advance-or-retract-to-point
    (kbd "<up>") 'fstar-subp-retract-last
    (kbd "<down>") 'fstar-subp-advance-next
    (kbd "<left>") 'fstar-subp-goto-beginning-of-unprocessed)
  (evil-define-key 'insert fstar-mode-map
    (kbd "<right>") 'fstar-subp-advance-or-retract-to-point
    (kbd "<up>") 'fstar-subp-retract-last
    (kbd "<down>") 'fstar-subp-advance-next
    (kbd "<left>") 'fstar-subp-goto-beginning-of-unprocessed)

  ;; leader keys
  (evil-set-leader 'normal (kbd "\\"))
  (evil-define-key 'normal 'global
    (kbd "<leader>m") 'menu-bar-open
    (kbd "<leader>n") 'neotree-toggle
    (kbd "<leader>f") 'neotree-find)
  ;; Coq
  (evil-define-key 'normal coq-mode-map
    (kbd "<leader>e") 'coq-double-hit-toggle
    (kbd "<leader>c") 'proof-interrupt-process)
  ;; Racket
  (evil-define-key 'normal racket-mode-map
    (kbd "<leader>r") 'racket-run
    (kbd "<leader>t") 'racket-test
    (kbd "<leader>k") 'racket-repl-clear-leaving-last-prompt)
  (evil-define-key 'normal racket-repl-mode-map
    (kbd "<leader>k") 'racket-repl-clear-leaving-last-prompt))

(use-package proof-site
  :load-path "vendor/PG/generic"

  :config
  ;; colors and display
  (if (display-graphic-p)
      ;; GUI
      ; red: #fec1c2, green: #e1fec1, blue: #c1fefc
      (custom-set-faces
       '(proof-locked-face ((t (:background "#e1fec1"))))
       '(proof-queue-face ((t (:background "#c1fefc")))))
      ;; CLI
      (custom-set-faces
       '(proof-locked-face ((t (:background "#262626")))) ; terminal
       '(proof-queue-face ((t (:background "#00005f"))))))

  (add-hook 'proof-mode-hook (lambda ()
                               (setq-local global-hl-line-mode
                                           (null global-hl-line-mode))))
  (add-hook 'coq-mode-hook #'company-coq-mode)

  (setf proof-colour-locked t)
  (setf overlay-arrow-string "")
  (setf proof-splash-enable nil)

  ;; Coq specific
  (setf coq-compile-before-require t)
  (setf coq-double-hit-enable t)
  (setf coq-one-command-per-line nil)) ; results in better behavior for electric terminator

(use-package editorconfig
  :load-path "vendor/editorconfig-emacs"

  :config
  (editorconfig-mode 1))

(use-package avy
  :load-path "vendor/avy"

  :config
  (setf avy-background t)
  (setf avy-all-windows nil)
  (setf avy-keys (append "asdghklqwertyuiopzxcvbnmfj;" nil)) ; same as vim-easymotion
  (if (display-graphic-p)
      ;; GUI
      (custom-set-faces
       '(avy-lead-face ((t (:foreground "#dc322f" :weight bold)))) ; red
       '(avy-lead-face-0 ((t (:foreground "#268bd2" :weight bold)))) ; blue
       '(avy-lead-face-2 ((t (:foreground "#268bd2" :weight bold))))) ; blue
      ;; CLI
      (custom-set-faces
       '(avy-lead-face ((t (:foreground "#d70000" :weight bold)))) ; red
       '(avy-lead-face-0 ((t (:foreground "#af8700" :weight bold)))) ; yellow
       '(avy-lead-face-2 ((t (:foreground "#af8700" :weight bold))))))) ; yellow

;; this needs to be loaded after avy
(use-package evil-easymotion
  :load-path "vendor/evil-easymotion"

  :config
  (let ((prefix "SPC"))
    (evilem-default-keybindings prefix)
    ;; redefine certain macros to allow matching across lines
    (evilem-define (kbd (concat prefix " w")) #'evil-forward-word-begin)
    (evilem-define (kbd (concat prefix " W")) #'evil-forward-WORD-begin)
    (evilem-define (kbd (concat prefix " e")) #'evil-forward-word-end)
    (evilem-define (kbd (concat prefix " E")) #'evil-forward-WORD-end)
    (evilem-define (kbd (concat prefix " b")) #'evil-backward-word-begin)
    (evilem-define (kbd (concat prefix " B")) #'evil-backward-WORD-begin)))

;; this needs to be loaded before fiplr
(use-package grizzl
  :load-path "vendor/grizzl")

;; this needs to be loaded after evil
(use-package fiplr
  :load-path "vendor/fiplr"

  :config
  (define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)
  ;; the fiplr-keymap may not be part of the public API, but this seems to work
  ;; for now
  (define-key *fiplr-keymap* (kbd "<f5>") 'fiplr-reload-list))

;; this needs to be loaded after evil
(use-package neotree
  :load-path "vendor/emacs-neotree"

  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter))))

(use-package company
  :load-path "vendor/company-mode"

  :config
  (global-company-mode))

(use-package math-symbol-lists
  :load-path "vendor/math-symbol-lists")

;; this needs to be loaded after company and math-symbol-lists
(use-package company-math
  :load-path "vendor/company-math")

(use-package yasnippet
  :load-path "vendor/yasnippet")

;; this needs to be loaded after company-math and yasnippet
(use-package company-coq
  :load-path "vendor/company-coq")

(use-package linum-relative
  :load-path "vendor/linum-relative"

  :init
  (global-linum-mode t)
  ;; add padding next to line number
  (setf linum-format
        (lambda (line)
          (propertize
           (format
            (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
              (concat "%" (number-to-string w) "d "))
            line)
           'face
           'linum)))

  :config
  (linum-relative-on)
  (setf linum-relative-format "%3s "
        linum-relative-current-symbol ""))

;; this needs to be loaded before racket-mode
(use-package pos-tip
  :load-path "vendor/pos-tip")

(use-package racket-mode
  :load-path "vendor/racket-mode")

;; this needs to be loaded after evil
(use-package evil-surround
  :load-path "vendor/evil-surround"
  :config
  (global-evil-surround-mode 1))

(use-package quick-peek
  :load-path "vendor/quick-peek")

(use-package flycheck
  :load-path "vendor/flycheck")

(use-package company-quickhelp
  :load-path "vendor/company-quickhelp")

(use-package dash
  :load-path "vendor/dash.el")

;; this depends on quick-peek, flycheck, company-quickhelp, dash
(use-package fstar-mode
  :load-path "vendor/fstar-mode.el"
  :mode ("\\.fst\\'" . fstar-mode))
