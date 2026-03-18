;;; format-org-json.el --- Pretty-print JSON in org-mode source blocks -*- lexical-binding: t; -*-

;; Author: Ian Whitlock
;; Description: Format JSON within #+begin_src json blocks in org-mode files

;;; Commentary:
;; 
;; This package provides functions to pretty-print JSON content within
;; org-mode source blocks. Can be used interactively or in batch mode.
;;
;; Interactive use:
;;   M-x iw/org-format-json-block     - Format JSON block at point
;;   M-x iw/org-format-all-json-blocks - Format all JSON blocks in buffer
;;
;; Batch use:
;;   emacs --batch -l format-org-json.el -f iw/batch-format-org-json FILE

;;; Code:

(require 'json)
(require 'org)

(defun iw/json-pretty-print-string (json-string)
  "Pretty-print JSON-STRING and return formatted string.
Returns original string if parsing fails."
  (condition-case err
      (let ((json-object-type 'alist)
            (json-array-type 'vector)
            (json-key-type 'string))
        (with-temp-buffer
          (insert json-string)
          (goto-char (point-min))
          (let ((parsed (json-read)))
            (erase-buffer)
            (insert (json-encode parsed))
            (json-pretty-print-buffer)
            (buffer-string))))
    (error
     (message "JSON parse error: %s" (error-message-string err))
     json-string)))

(defun iw/org-format-json-block ()
  "Pretty-print JSON in the source block at point."
  (interactive)
  (when (org-in-src-block-p)
    (let* ((element (org-element-at-point))
           (lang (org-element-property :language element))
           (begin (org-element-property :begin element))
           (end (org-element-property :end element)))
      (when (string= lang "json")
        (save-excursion
          (goto-char begin)
          (when (re-search-forward "#\\+begin_src json\n" end t)
            (let ((content-start (point)))
              (when (re-search-forward "^[ \t]*#\\+end_src" end t)
                (let* ((content-end (match-beginning 0))
                       (content (buffer-substring-no-properties content-start content-end))
                       (trimmed (string-trim content))
                       (formatted (iw/json-pretty-print-string trimmed)))
                  (unless (string= trimmed formatted)
                    (delete-region content-start content-end)
                    (goto-char content-start)
                    (insert formatted "\n")))))))))))

(defun iw/org-format-all-json-blocks ()
  "Pretty-print all JSON source blocks in current buffer."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_src json" nil t)
        (iw/org-format-json-block)
        (setq count (1+ count))))
    (message "Formatted %d JSON blocks" count)
    count))

(defun iw/batch-format-org-json ()
  "Batch function to format JSON blocks in files from command line.
Usage: emacs --batch -l format-org-json.el -f iw/batch-format-org-json FILE..."
  (dolist (file command-line-args-left)
    (when (file-exists-p file)
      (message "Processing: %s" file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (let ((count (iw/org-format-all-json-blocks)))
          (when (> count 0)
            (write-region (point-min) (point-max) file)
            (message "Wrote %s with %d formatted blocks" file count))))))
  (setq command-line-args-left nil))

(provide 'format-org-json)
;;; format-org-json.el ends here
