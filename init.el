;; Setup basic variables
(defvar user-home-dir (getenv "HOME"))
(defvar conf-root-dir (concat user-home-dir "/.emacs.d"))

(add-to-list 'load-path (concat conf-root-dir "/etc-lisp"))
(require 'ww-init)
(require 'ww-package)
(require 'ww-org-mode)
