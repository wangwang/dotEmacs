(require 'ww-init)

;; {{{ initialize emacs24 package-management
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;; }}}

;; {{{
;;(unless (require 'el-get nil t)
;;  (url-retrieve
;;   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
;;   (lambda (s)
;;     (end-of-buffer)
;;     (eval-print-last-sexp))))
;; }}}

;; {{{ setup el-get
(add-search-path "site-lisp/el-get")
(require 'el-get)
;; }}}

;; set local recipes, el-get-sources should only accept PLIST element
(setq el-get-sources
  '(
    (:name css-mode :type emacswiki)
    (:name magit
        :after (progn
                (global-set-key (kbd "C-x C-z") 'magit-status)))))


;; now set our own packages
(setq ww-packages (append '(
                             ;; some prerequisites
                             smart-operator
                             autopair
                             xml-rpc-el
                             metaweblog ;; must be placed after xml-rpc-el

                             ahg
                             anything
                             auctex
                             boxquote
                             popup
                             auto-complete
                             auto-complete-clang
                             auto-complete-css
                             auto-complete-emacs-lisp
                             auto-complete-ruby
                             auto-complete-yasnippet
                             color-theme
                             color-theme-ir-black
                             color-theme-tango
                             crontab-mode
                             css-mode
                             cssh
                             dash
                             ecb
                             edit-server
                             ess
                             emacs-w3m
                             flim
                             flycheck
                             fuzzy
                             go-mode
                             google-c-style
                             google-maps
                             haskell-mode
                             hexrgb
                             highlight-parentheses
                             htmlize
                             inf-ruby
                             jedi
                             lorem-ipsum
                             magit
                             markdown-mode
                             ;mu4e
                             nxhtml
                             org-mode
                             org-jekyll
                             org-impress-js
                             puppet-mode
                             pymacs
                             python-mode
                             ruby-compilation
                             ruby-mode
                             session
                             slime
                             switch-window
                             ;; virtualenv
                             vkill
                             xcscope
                             yaml-mode
                             org2blog    ;; must be placed after xml-rpc-el & metaweblog
                             o-blog
                             popup
                             tail
                             tomorrow-theme
                             yasnippet
                             weblogger-el
                             window-numbering
                             wrap-region)(mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync ww-packages)

(provide 'ww-package)
