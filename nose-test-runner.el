;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'file-change-monitor)

(setq lexical-binding t)

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
  "Color the mode-line to show an error has occurred."
  (set-face-background 'mode-line error-color)
  (setq mode-line-format
        (append *original-mode-line* '("ERR"))))

(defun display-success ()
  "Color the mode-line to show tests completed successful."
  (set-face-background 'mode-line success-color)
  (setq mode-line-format
        (append *original-mode-line* '("SUCC"))))

(defun display-unknown ()
  "Color the mode-line indeterminate."
  (set-face-background 'mode-line unknown-color)
  (setq mode-line-format
        (append *original-mode-line* '("UNK"))))

(defun test-run-finished (process msg)
  "Check the test output and color the mode-line accordingly."
  (cond ((string-match fail-regex msg) (display-error))
        ((string-match success-regex msg) (display-success))
        (:else (display-unknown))))

(defun test-func (cmd params workingdir)
  "Run the test-cmd (AS CMD) and send output to 'test-run-finished function.
Further parameters as PARAMS.
Working dir to use as WORKINGDIR."
  (let* ((test-dir (expand-file-name workingdir))
         (arguments (append params (list test-dir)))
         (proc-name "test-proc")
         (proc-buffer "*tests*")
         (proc (apply 'start-process proc-name proc-buffer cmd arguments)))
    (set-process-filter proc 'test-run-finished)))

;; This is specific to the project and should be put elsewhere.

(defun test-nose-func (process msg)
  ""
  (test-func "nosetests" '("--exe") "~/Code/Python/opencv_web_service/mapper/tests/"))

(defun start-monitor-nose (directory)
  "Start a nosetest monitor on the given directory (AS DIRECTORY)."
  (let ((filepath (expand-file-name directory))
        (os system-type))
    (cond ((eq 'gnu/linux os)
           (install-monitor-linux filepath 'test-nose-func))
          ((t (install-monitor filepath 5 (lambda () (test-nose-func nil nil))))))))

(start-monitor-nose "~/Code/Python/opencv_web_service/mapper/tests/")
;;; nose-test-runner.el ends here
