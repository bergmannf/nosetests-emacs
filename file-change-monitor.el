;;; packagse --- Summary:
;;; Provide file watching functionality for elisp.
;;; Commentary:
;;; Code:
;;; - Provides a special implementation for linux that uses inotifywait.
;;; - Pretends to monitor a file by checking it every 5 seconds if
;;; inotifywait is not available.

(defun install-monitor-linux (file func)
  "Function using inotifywait to monitor the file system and only run
the associated function when changes are detected."
  (if (executable-find "inotifywait")
      (let* ((cmd "inotifywait")
              (filepath (expand-file-name file))
              (arguments (list "-m" "-e" "create" filepath))
              (process (apply 'start-process "NOTIFYWAIT" "*notifywait*" cmd arguments)))
         (set-process-filter process func))
    (install-monitor file 5 func)))

(defun install-monitor (file secs func)
  "Pretend to monitor the given file (AS FILE) by issuing a check every secs (AS SECS) seconds.
If a change in `file-attributes` happended call func."
  (let ((monitor-attributes (file-attributes file))
        (fun func))
    (run-with-timer
     0 secs
     (lambda (f p)
       (let ((att (file-attributes f)))
         (unless (or (null monitor-attributes) (equalp monitor-attributes att))
           (funcall fun))
         (setq monitor-attributes att)))
     file secs)))

(provide 'file-change-monitor)
;;; file-change-monitor.el ends here
