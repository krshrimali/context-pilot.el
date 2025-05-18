;;; contextpilot.el --- Emacs plugin for ContextPilot -*- lexical-binding: t; -*-

(defvar contextpilot-command "contextpilot"
  "The contextpilot CLI command path.")

(defun contextpilot--project-root ()
  "Get the root of the current project."
  (or (when (fboundp 'project-root)
        (when-let ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun contextpilot--run (args callback)
  "Run contextpilot with ARGS and call CALLBACK with output lines.
Runs silently and kills the temp process buffer after use."
  (let ((buf (generate-new-buffer " *contextpilot-process*")))
    (make-process
     :name "contextpilot"
     :buffer buf
     :command (split-string (concat contextpilot-command " " args))
     :noquery t
     :stderr buf
     :sentinel
     (lambda (_proc event)
       (when (string= event "finished\n")
         (with-current-buffer buf
           (let ((output (buffer-substring-no-properties (point-min) (point-max))))
             (let ((lines (mapcar (lambda (line)
                                    (string-trim-right line "[\r\n]+"))
                                  (split-string output "\n" t))))
               (funcall callback lines))))
         (kill-buffer buf))))))

(defun contextpilot--pick-file (title lines)
  "Use completing-read to pick a file from LINES and open it."
  (let ((choice (completing-read (concat title ": ") lines)))
    (when (string-match "\\(.*\\) (\\([0-9]+\\) occurrences)" choice)
      (find-file (match-string 1 choice)))))

(defun contextpilot--query (file start end title)
  "Run a query from START to END lines in FILE."
  (let ((args (format "%s -t query %s -s %d -e %d"
                      (contextpilot--project-root)
                      file start end)))
    (contextpilot--run args
                       (lambda (lines)
                         (let ((entries (mapcar (lambda (line)
                                                  (if (string-match " (.*occurrences)" line)
                                                      line
                                                    (concat line " (0 occurrences)")))
                                                lines)))
                           (contextpilot--pick-file title entries))))))

;;;###autoload
(defun contextpilot-query-whole-file ()
  "Query the whole file for top related files."
  (interactive)
  (let ((file (buffer-file-name)))
    (contextpilot--query file 1 0 "Top Files for Whole File")))

;;;###autoload
(defun contextpilot-query-current-line ()
  "Query only the current line."
  (interactive)
  (let ((file (buffer-file-name))
        (line (line-number-at-pos)))
    (contextpilot--query file line line (format "Top Files for Line %d" line))))

;;;###autoload
(defun contextpilot-query-range (start end)
  "Query the selected region."
  (interactive "r")
  (let ((file (buffer-file-name))
        (start-line (line-number-at-pos start))
        (end-line (line-number-at-pos end)))
    (contextpilot--query file start-line end-line
                         (format "Top Files (%d-%d)" start-line end-line))))

;;;###autoload
(defun contextpilot-index-workspace ()
  "Index the project workspace."
  (interactive)
  (contextpilot--run (format "%s -t index" (contextpilot--project-root))
                     (lambda (_lines)
                       (message "✅ Indexing complete."))))

;;;###autoload
(defun contextpilot-descriptions-for-range (start end)
  "Fetch and show descriptions for the selected region."
  (interactive "r")
  (let* ((file (buffer-file-name))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (args (format "%s -t desc %s -s %d -e %d"
                       (contextpilot--project-root)
                       file start-line end-line)))
    (contextpilot--run args
                       (lambda (lines)
                         (condition-case err
                             (let* ((json-str (car lines))
                                    (parsed (json-parse-string json-str :array-type 'list)))
                               (if (null parsed)
                                   (message "No descriptions found.")
                                 (let* ((titles (mapcar #'car parsed))
                                        (choice (completing-read "Choose description: " titles))
                                        (desc (cadr (seq-find (lambda (x) (string= (car x) choice)) parsed))))
                                   (run-at-time
                                    0.01 nil
                                    (lambda ()
                                      (let* ((bufname (format "*ContextPilot: %s*" choice))
                                             (buf (generate-new-buffer bufname)))
                                        (with-current-buffer buf
                                          (erase-buffer)
                                          (let ((text (cond
                                                       ((stringp desc) desc)
                                                       ((and (listp desc) (= (length desc) 1) (stringp (car desc))) (car desc))
                                                       (t nil))))
                                            (if text
                                                (insert (string-trim-right text))
                                              (insert (format "⚠️ Invalid or malformed description for: %s\n\nRaw: %S"
                                                              choice desc))))
                                          (goto-char (point-min))
                                          (markdown-mode)) ;; or gfm-mode
                                        (display-buffer-in-side-window
                                         buf '((side . right)
                                               (window-width . 0.5)
                                               (slot . 1))))))))
                               (error
                                (let ((errbuf (get-buffer-create "*ContextPilot Error*")))
                                  (with-current-buffer errbuf
                                    (erase-buffer)
                                    (insert (format "[ContextPilot] JSON parse error:\n\n%s\n\nRaw output:\n\n%s"
                                                    (error-message-string err)
                                                    (mapconcat #'identity lines "\n")))
                                    (goto-char (point-min))
                                    (text-mode))
                                  (display-buffer errbuf)))))))))

(provide 'contextpilot)
;;; contextpilot.el ends here
