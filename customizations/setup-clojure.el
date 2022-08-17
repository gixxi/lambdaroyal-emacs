(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)



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
