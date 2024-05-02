;; customizations/print-templating-mode.el

(require 'cider)

(defvar-local print-templating-datatype nil
  "Datatype to use for the print templating evaluation.")

(defvar-local print-templating-id nil
  "ID to use for the print templating evaluation.")


(with-eval-after-load 'cider

  (defun evaluate-clojure-function-and-display ()
    "Evaluate a predefined Clojure function using the file associated with the buffer as the template and display the results in another buffer."
    (interactive)
    (let ((code (format "(do
                        (require 
                         '[lambdaroyal.memory.core.tx :as tx]
                         '[lambdaroyal.vlic.ioc :refer :all]
                         '[lambdaroyal.vlic.crosscutting.print-templating :as pt])
                        (with-tx @lambdaroyal.vlic.main/system
                          (let [datatype (keyword \"%s\")
                                id %s
                                template-filename \"%s\"]
                            (let [template (slurp template-filename)
                                  record (tx/select-first *tx* datatype id)]
                              (binding [pt/*x* record
                                        pt/*escape-character-lambda* pt/escape-latex-characters]
                                 (pt/run* (pt/read* template)))))))"
                        print-templating-datatype
                        print-templating-id
                        (buffer-file-name))))
      (cider-interactive-eval code
                              (nrepl-make-response-handler (current-buffer)
                                                           (lambda (buffer value)
                                                             (with-current-buffer (get-buffer-create "*Clojure Output*")
                                                               (read-only-mode 0)
                                                               (erase-buffer)
                                                               ;; Replace escaped newlines and tabs
                                                               (let ((formatted-value (replace-regexp-in-string "\\\\n" "\n" value)))
                                                                 (setq formatted-value (replace-regexp-in-string "\\\\t" "\t" formatted-value))
                                                                 (insert formatted-value))
                                                                                                                            
                                                               (read-only-mode 1)
                                                               (display-buffer (current-buffer))))
                                                           nil nil nil))))


  (define-minor-mode print-templating-mode
    "A minor mode for evaluating Clojure templates on buffer save."
    :lighter " PT"
    :global nil
    (if print-templating-mode
        (print-templating-enable)
      (print-templating-disable)))

  (defun print-templating-enable ()
    "Enable print templating mode, asking user for datatype and ID, and evaluate immediately."
    (interactive)
    (setq print-templating-datatype (read-string "Enter datatype: "))
    (setq print-templating-id (read-string "Enter ID: "))
    (add-hook 'after-save-hook #'evaluate-clojure-function-and-display nil t)
    (evaluate-clojure-function-and-display)  ; Evaluate immediately after setup
    (message "Print Templating mode enabled and evaluated."))


  (defun print-templating-disable ()
    "Disable print templating mode."
    (interactive)
    (remove-hook 'after-save-hook #'evaluate-clojure-function-and-display t)
    (message "Print Templating mode disabled."))


  ;; Define your enabling and disabling functions here as well.
  )

