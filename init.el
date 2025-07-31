;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))



;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.

;; 20221103 avoid this due to recent/current MELPA performance issues
;;(when (not package-archive-contents)
;;  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider
    ac-cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-completing-read+

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    projectile

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    solarized-theme
    spacemacs-theme
    js2-mode
    web-mode
    tide
    org-bullets
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")
(load "green_theme.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

(load "setup-js.el")

(load "setup-clojure.el")
(load "setup-org.el")
(load "xsltproc-mode.el")
(load "xslt-preview.el")
(load "print-templating-mode.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8"
           "#70c0b1" "#000000"))
 '(coffee-tab-width 2)
 '(custom-safe-themes
   '("58c996beb973f7e988ee4fd21c367b7a5bbdb0622ddfbbd112672a7b4e3d3b81"
     "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692"
     "306d853c5b47e1baf5e815eb49daa8a46d7f54d3f5ab624f3b30a6c1eb8e1f0c"
     "46c65f6d9031e2f55b919b1486952cddcc8e3ee081ade7eb2ffb6a68a804d30e"
     "84c2c93ce268699838b51eeeaaec46e064d5048e232b39113b73209be3ef6fd4"
     "c2efe6f5e2bd0bddfb2d6e26040545768939d2029f77e6b6a18d1ee0e0cb1033"
     "74367676a7b0562975704f8e576d5e103451527b36c9226a013cd8f3ae2140f5"
     "dbc6d947d551aa03090daf6256233454c6a63240e17a8f3d77889d76fef1749d"
     "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "72cc2c6c5642b117034b99dcc3a33ff97a66593429c7f44cd21b995b17eebd4e"
     "e16cd3e6093cf35e65dbac6c7649931936f32f56be34477cb7cbe1ee332c5b99"
     "b64a60e69617b4348d0402fad2f0d08a694301132e7ab243dab4d6a65c3bf948"
     "cd65fad3243fb2b04660fb5c56152e27030e904b5f06b743bb77cac85c5327b7"
     "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f"
     "b3a0f4b6792c96382caf56ad55c78f3ec2ffdd78578bbed36dbf770b9b132eed"
     "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9"
     "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c"
     "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc"
     "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1"
     "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd"
     "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78"
     default))
 '(fci-rule-color "#2a2a2a")
 '(package-selected-packages
   '(ac-cider aggressive-indent cider cider-hydra
              clojure-mode-extra-font-locking clojure-ts-mode
              ef-themes green-is-the-new-black-theme grip-mode
              ido-ubiquitous js2-mode magit markdown-mode
              markdown-preview-mode moe-theme monokai-theme paredit
              popup-complete projectile psgml rainbow-delimiters
              rainbow-mode smex solarized-theme soothe-theme
              spacemacs-theme tagedit tide typescript-mode
              vs-light-theme yaml-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#b2b2b2" :family "Sans Serif" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#b2b2b2" :family "Sans Serif" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#b2b2b2" :family "Sans Serif" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#b2b2b2" :family "Sans Serif" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#b2b2b2" :family "Sans Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#b2b2b2" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#b2b2b2" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#b2b2b2" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#b2b2b2" :family "Sans Serif"))))
 '(show-paren-match ((t (:background "#000000" :foreground "red" :weight bold)))))
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
