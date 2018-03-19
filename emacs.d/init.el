(add-to-list 'load-path "~/.emacs.d/lisp")

(load "personal/packages")
(load "personal/theme")
(load "personal/settings")
(load "personal/functions")

(let ((custom "~/.emacs_local"))
  (when (file-exists-p custom)
    (load-file custom)))
