;;; le-janet.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  John Bonini
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'janet-mode)                   ;; janet-ts-mode?
(require 'ajrepl)

(declare-function ajrepl-trim-trailing-newline-maybe "ext:ajrepl")
(declare-function ajrepl-get-process "ext:ajrepl")

(defun lispy--eval-janet (code-str)
  "Eval CODE-STR as janet code. Return repl output."
  (with-current-buffer ajrepl-repl-buffer-name
    (goto-char (point-max))
    (insert (ajrepl-trim-trailing-newline-maybe code-str))
    (comint-send-input)
    (let ((command-output-begin (point))
          (response-timeout 5))
      (while (null (save-excursion
                     (let ((inhibit-field-text-motion t))
                       (goto-char command-output-begin)
                       (re-search-forward "^repl:[0-9]*:> " nil t))))
        (accept-process-output (ajrepl-get-process) response-timeout)
        (goto-char (point-max)))
      (goto-char (point-max))
      (buffer-substring-no-properties command-output-begin (line-end-position 0)))))

(provide 'le-janet)
;;; le-janet.el ends here
