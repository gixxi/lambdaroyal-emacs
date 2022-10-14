(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'cider-repl-mode-hook
          '(lambda ()
             (define-key cider-repl-mode-map "{" #'paredit-open-curly)
             (define-key cider-repl-mode-map "}" #'paredit-close-curly)))


(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(require 'auto-complete)
(global-auto-complete-mode t)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(setq cider-print-options '(("level" 3) ("length" 10)))



(defun my-cider-stacktrace-render-cause (buffer cause num note)
    "Emit into BUFFER the CAUSE NUM, exception class, message, data, and NOTE."
    (with-current-buffer buffer
      (nrepl-dbind-response cause (class message data spec stacktrace)
        (let ((indent "   ")
              (class-face 'cider-stacktrace-error-class-face)
                 (message-face 'cider-stacktrace-error-message-face))
          (cider-propertize-region `(cause ,num)
            ;; Detail level 0: exception class
            (cider-propertize-region '(detail 0)
              (insert (format "%d. " num)
                      (propertize note 'font-lock-face 'font-lock-comment-face) " "
                      (propertize class 'font-lock-face class-face)
                      "\n"))
            ;; Detail level 1: message + ex-data
            (cider-propertize-region '(detail 1)
              (if (equal class "clojure.lang.Compiler$CompilerException")
                  (cider-stacktrace-render-compile-error buffer cause)
                (cider-stacktrace-emit-indented
                 (propertize
                  (truncate-string-to-width (or message "(No message)") 100)
                  'font-lock-face  message-face)
                 indent t))
              (insert "\n")
              (when spec
                   (cider-stacktrace--emit-spec-problems spec (concat indent "  ")))
              (when data
                (cider-stacktrace-emit-indented data indent nil t)))
            ;; Detail level 2: stacktrace
            (cider-propertize-region '(detail 2)
              (insert "\n")
              (let ((beg (point))
                    (bg `(:background ,cider-stacktrace-frames-background-color :extend t)))
                (dolist (frame stacktrace)
                  (cider-stacktrace-render-frame buffer frame))
                (overlay-put (make-overlay beg (point)) 'font-lock-face bg)))
            ;; Add line break between causes, even when collapsed.
            (cider-propertize-region '(detail 0)
              (insert "\n")))))))

(advice-add 'cider-stacktrace-render-cause :override #'my-cider-stacktrace-render-cause)
(setq cider-repl-buffer-size-limit 10000)
