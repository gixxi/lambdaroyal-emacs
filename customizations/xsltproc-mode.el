(defvar xsltproc-output-buffer-name "*xsltproc-output*"
  "Name of the buffer that will display the xsltproc output.")

(make-variable-buffer-local
 (defvar xsltproc-xml-file nil
   "Path to the XML file to use for the XSLT transformation."))

(make-variable-buffer-local
 (defvar xsltproc-xslt-file nil
   "Path to the XSLT file to use for the XSLT transformation."))

(defmacro el-comment (&rest body)
  "Ignore BODY completely during evaluation."
  nil)

(defun xsltproc-apply (xml-file xslt-file)
  "Run xsltproc with XML-FILE and XSLT-FILE, and display the result in a buffer."
  (when (and xml-file xslt-file)
    (let ((output-file (make-temp-file "xsltproc-output" nil ".xml"))
          (error-file (make-temp-file "xsltproc-error" nil ".txt"))
          (output-buffer (get-buffer-create xsltproc-output-buffer-name))
          (error-buffer (get-buffer-create "*xsltproc-errors*")))
      (message (format "Output File %s" output-file))
      (shell-command (format "xsltproc -o %s %s %s 2>%s" output-file xslt-file xml-file error-file))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert-file-contents output-file)
        (set-visited-file-name nil)
        (set-buffer-modified-p nil)
        (xml-mode))
      (display-buffer output-buffer)
      (with-current-buffer error-buffer
        (erase-buffer)
        (insert-file-contents error-file))
      (if (> (buffer-size error-buffer) 0)
          (display-buffer error-buffer)
        (kill-buffer error-buffer))
      (el-comment (with-current-buffer output-buffer
                    (erase-buffer)
                    (insert-file-contents output-file)
                    (set-visited-file-name nil)
                    (set-buffer-modified-p nil)
                    (xml-mode))
                  (display-buffer output-buffer)
                  (with-current-buffer error-buffer
                    (erase-buffer)
                    (insert-file-contents error-file))
                  (if (> (buffer-size error-buffer) 0)
                      (display-buffer error-buffer)
                    (kill-buffer error-buffer)))
      (message "Applied XSLT transformation"))))


(defun xsltproc-on-save ()
  "Run xsltproc-apply when an XML or XSLT file is saved."
  (when xsltproc-mode
    (let ((file-name (buffer-file-name)))
      (when (and file-name (string-match-p "\\.\\(xml\\|xslt\\)$" file-name))
        (unless xsltproc-xml-file
          (setq xsltproc-xml-file (read-file-name "Select XML file:")))
        (unless xsltproc-xslt-file
          (setq xsltproc-xslt-file (read-file-name "Select XSLT file:")))
        (xsltproc-apply xsltproc-xml-file xsltproc-xslt-file)))))

(define-minor-mode xsltproc-mode
  "Toggle XSLT processing on save."
  :init-value nil
  :lighter " XSLT"
  (if xsltproc-mode
      (add-hook 'after-save-hook #'xsltproc-on-save nil t)
    (remove-hook 'after-save-hook #'xsltproc-on-save t)))

(defun xsltproc-clear-files ()
  "Clear the buffer-local XML and XSLT filenames."
  (interactive)
  (setq xsltproc-xml-file nil)
  (setq xsltproc-xslt-file nil))
