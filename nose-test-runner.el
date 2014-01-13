;;; package --- Summary
;;; Commentary:
;;; Code:

(defvar *original-mode-line* mode-line-format)

;; Coloring the mode-line seems a bit off:
;; - Only color it in a Python buffer.
;; - Only color a tiny sqaure to indicate the change.

(defcustom fail-regex "FAILED" "Regex that indicates a failed testrun."
  :group 'nose-monitor)
(defcustom success-regex "OK" "Regex that indicates a successful testrun."
  :group 'nose-monitor)
(defcustom success-color "DarkGreen" "Indicator color when all tests pass."
  :group 'nose-monitor)
(defcustom error-color "DarkRed" "Indicator color when tests fail."
  :group 'nose-monitor)
(defcustom unknown-color "#4477aa" "Indicator color when outcome is neither success nor failure."
  :group 'nose-monitor)

(defun display-error ()
  ""
  (set-face-background 'mode-line error-color)
  (setq mode-line-format
        (append *original-mode-line* '("ERR"))))

(defun display-success ()
  ""
  (set-face-background 'mode-line success-color)
  (setq mode-line-format
        (append *original-mode-line* '("SUCC"))))

(defun display-unknown ()
  ""
  (set-face-background 'mode-line unknown-color)
  (setq mode-line-format
        (append *original-mode-line* '("UNK"))))

(defun test-run-finished (process msg)
  ""
  (cond ((string-match fail-regex msg) (display-error))
        ((string-match success-regex msg) (display-success))
        (:else (display-unknown))))

(defun test-func (cmd params workingdir)
  "Run the nosetests function and send output to 'nose-indicator.
Further parameters as PARAMS.
Working dir to use as WORKINGDIR."
  (let* ((test-dir (expand-file-name workingdir))
         (joined-params (append params (list test-dir)))
         (proc-name "test-proc")
         (proc-buffer "*tests*")
         (proc (apply 'start-process proc-name proc-buffer cmd joined-params)))
    (set-process-filter proc 'test-run-finished)))

;; This is specific to the project and should be put elsewhere.

(defun test-nose-func ()
  ""
  (test-func "nosetests" '("--exe") "~/Code/Python/opencv_web_service/mapper/tests/"))

(defun install-monitor (file secs func)
  (lexical-let ((monitor-attributes (file-attributes file))
                (fun func))
    (run-with-timer
     0 secs
     (lambda (f p)
       (let ((att (file-attributes f)))
         (unless (or (null monitor-attributes) (equalp monitor-attributes att))
           (funcall fun))
         (setq monitor-attributes att)))
     file secs)))

(setq monitor-timer (install-monitor
                     (expand-file-name "~/Code/Python/opencv_web_service/mapper/tests/")
                     5
                     'test-nose-func))
;;; monitor-dir.el ends here
