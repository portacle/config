;;;;; ＤＯ　ＮＯＴ　ＭＯＤＩＦＹ　ＴＨＩＳ　ＦＩＬＥ！
;;
;; This file will be replaced whenever Portacle is updated.
;; For emacs configuration changes, please use the dedicated
;; user.el file as explained in the help buffer.
;; 

;; Predefine bare minimum path functions
(setq portacle-root (or (getenv "ROOT") (expand-file-name "~/")))
(defun portacle-path (path)
  (concat portacle-root path))

;; Set up necessary paths
(setq user-emacs-directory (portacle-path "all/emacsd/"))
(add-to-list 'load-path (portacle-path "all/emacsd/portacle/"))
(cd portacle-root)

;; Load main library
(if (locate-library "portacle")
    (load-library "portacle")
    (display-warning :warning "Basic Portacle scripts are not present."))
