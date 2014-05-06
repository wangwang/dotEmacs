(message (concat "ww-init: user-home-dir: " user-home-dir))
(message (concat "ww-init: conf-root-dir: " conf-root-dir))

;; define basic function
(defun add-search-path (path)
  (add-to-list 'load-path (concat conf-root-dir "/" path))
  (message (concat "ww-init: load-path added: " path)))

; reload dotemacs
(defun ww-reload ()
  (interactive)
  (load-file "~/.emacs.d/init.el") (message "dotEmacs reloaded successfully"))

;; load basic search path
;;(add-search-path "etc-lisp") ;; already added in init.el
(add-search-path "site-lisp")
(add-search-path "el-get")
(provide 'ww-init)
