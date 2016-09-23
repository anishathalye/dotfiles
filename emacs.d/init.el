(add-to-list 'load-path "~/.emacs.d/lisp")

(load "personal/packages")
(load "personal/theme")
(load "personal/settings")

(let ((custom "~/.emacs_local"))
  (when (file-exists-p custom)
    (load-file custom)))
