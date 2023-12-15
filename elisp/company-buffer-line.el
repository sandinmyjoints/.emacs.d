;; from https://github.com/tungd/dotfiles/commit/a92dbe5b76310ca90c2662907c96acf755e04a71#diff-432cbc43198b3a1144c2bec5b63f631b18b23746b86bb55fb942f34bac3331bdR203
;; and https://github.com/tungd/dotfiles/commit/a92dbe5b76310ca90c2662907c96acf755e04a71#diff-432cbc43198b3a1144c2bec5b63f631b18b23746b86bb55fb942f34bac3331bdR203
(require 'cl-lib)
(require 'company)
(require 's)
(require 'seq)

(defun company-buffer-line--buffer-lines (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (mapcar #'s-trim
              (split-string (buffer-string) "[\n\r]")))))

(defun company-buffer-line--same-mode-buffers (&optional mode)
  (let ((mode (or mode major-mode)))
    (message (format "company-buffer-line--same-mode-buffers: inferred mode %s" mode))
    (cl-loop for buffer in (buffer-list)
             if (with-current-buffer buffer
                  (eq major-mode mode))
             collect buffer)))
;; (company-buffer-line--same-mode-buffers 'js2-jsx-mode)
;; (company-buffer-line--same-mode-buffers 'js2-mode)

(defun company-current-buffer-lines (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-current-buffer-lines))
    (prefix (when (looking-back "^\s*\\(.+\\)" (line-beginning-position))
              (match-string-no-properties 1)))
    (candidates (all-completions arg (seq-uniq (company-buffer-line--buffer-lines))))
    (sorted t)))

(defun company-same-mode-buffer-lines (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (message (format "company-same-mode-buffer-lines: called with command %s" command))
  (cl-case command
    (prefix
     (message (format "prefix: %s" (when (looking-back "^\s*\\(.+\\)" (line-beginning-position))
                                     (match-string-no-properties 1)))))
    (candidates
     (let* ((buffers (company-buffer-line--same-mode-buffers))
            (lines (seq-mapcat #'company-buffer-line--buffer-lines buffers)))
       (setq wjb-buffers buffers)
       (setq wjb-lines lines))))
  (cl-case command
    (interactive (company-begin-backend 'company-same-mode-buffer-lines))
    (prefix (when (looking-back "^\s*\\(.+\\)" (line-beginning-position))
              (match-string-no-properties 1)))
    (candidates (let* ((buffers (company-buffer-line--same-mode-buffers))
                       (lines (seq-mapcat #'company-buffer-line--buffer-lines buffers)))
                  (all-completions arg (seq-uniq lines))))
    (sorted t)))

(provide 'company-buffer-line)
