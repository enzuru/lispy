;;; le-lisp.el --- lispy support for Common Lisp. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Oleh Krehel

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(declare-function slime-output-buffer "ext:slime-repl")
(declare-function slime "ext:slime")
(declare-function slime-current-connection "ext:slime")
(declare-function slime-eval "ext:slime")
(declare-function slime-edit-definition "ext:slime")
(declare-function sly-mrepl--find-buffer "ext:sly-mrepl")
(declare-function sly "ext:sly")
(declare-function sly-current-connection "ext:sly")
(declare-function sly-eval "ext:sly")
(declare-function sly-edit-definition "ext:sly")

(defcustom lispy-use-sly nil
  "Whether to use SLY instead of SLIME."
  :group 'lispy
  :type 'boolean)

(defun lispy--use-sly-p ()
  (if lispy-use-sly
      (require 'sly)
    (unless (require 'slime nil t)
      (require 'sly)
      (setq lispy-use-sly t))))

(defun lispy--eval-lisp (str)
  "Eval STR as Common Lisp code."
  (let* ((deactivate-mark nil)
         (result (if (lispy--use-sly-p)
                     (with-current-buffer (process-buffer (lispy--cl-process))
                       (sly-eval `(slynk:eval-and-grab-output ,str)))
                   (slime-eval `(swank:eval-and-grab-output ,str)))))
    (pcase result
      (`("" "") "(ok)")
      (`("" ,val) val)
      (`(,out ,val)
       (concat (propertize (string-trim-left out) 'face 'font-lock-string-face) "\n\n" val)))))

(defun lispy--cl-process ()
  (unless (lispy--use-sly-p)
    (require 'slime-repl))
  (or (if (lispy--use-sly-p)
          (sly-current-connection)
        (slime-current-connection))
      (let (conn)
        (let ((wnd (current-window-configuration)))
          (if (lispy--use-sly-p)
              (sly)
            (slime))
          (while (not (if (lispy--use-sly-p)
                          (and (setq conn (sly-current-connection))
                               (sly-mrepl--find-buffer conn))
                        (and
                         (setq conn (slime-current-connection))
                         (get-buffer-window (slime-output-buffer)))))
            (sit-for 0.2))
          (set-window-configuration wnd)
          conn))))

(defun lispy--find-lisp-package (package-name)
  "Return either a CL expression to find the given package, or if
PACKAGE-NAME is nil the package we found in the Lisp buffer."
  (let ((package-via-buffer
         (upcase (string-replace
                  "#:" ""
                  (if (lispy--use-sly-p)
                      (sly-current-package)
                    (slime-current-package))))))
    (if (null package-name)
        package-via-buffer
      ;; The package local nickname is either defined in our current package or
      ;; from a different "home package". In case of the latter we can simply
      ;; rely on `cl:define-package' to figure it out for us. Note that we use
      ;; `cl:ignore-errors' for when a package either can't be found or might
      ;; not have been loaded yet.
      `(cl:or (cl:ignore-errors
               (,(if (lispy--use-sly-p) 'slynk-backend:find-locally-nicknamed-package
                   'swank/backend:find-locally-nicknamed-package)
                ,(upcase package-name)
                (cl:find-package ,package-via-buffer)))
              (cl:find-package ,(upcase package-name))))))

(defun lispy--lisp-args (symbol)
  "Return a pretty string with arguments for SYMBOL."
  (if-let* ((lisp-arglist
             (if (lispy--use-sly-p)
                 (sly-eval
                  `(slynk:operator-arglist
                    ,(sly-cl-symbol-name symbol)
                    ,(lispy--find-lisp-package (sly-cl-symbol-package symbol))))
               (slime-eval
                `(swank:operator-arglist
                  ,(slime-cl-symbol-name symbol)
                  ,(lispy--find-lisp-package (slime-cl-symbol-package symbol))))))
            (args (list (mapconcat #'prin1-to-string (read lisp-arglist) " "))))
      (let* ((symbol-package (if (lispy--use-sly-p) (sly-cl-symbol-package symbol)
                               (slime-cl-symbol-package symbol)))
             (package-prefixed-arglist (format "(%s:" symbol-package)))
        ;; In Lisp, it is often the case we prefix low level packages with a `%'
        ;; symbol. This is problematic in Elisp with `format'. For example, `%g'
        ;; can have special meaning. We can eliminate this edge case by always
        ;; concatenating.
        (concat
         (if symbol-package package-prefixed-arglist "(")
         (format "%s"
                 (mapconcat
                  #'identity
                  (mapcar (lambda (x)
                            (propertize (downcase x)
                                        'face 'lispy-face-req-nosel))
                          args)
                  (concat "\n" (make-string (+ 2 (length symbol)) ?\ ))))
         ")"))
    (format "Could not find symbol %s" (upcase symbol))))

(defun lispy--lisp-describe (symbol)
  "Return documentation for SYMBOL."
  (read
   (lispy--eval-lisp
    (substring-no-properties
     (format
      "(let ((x '%s))
        (or (if (boundp x)
                (documentation x 'variable)
              (documentation x 'function))
            \"undocumented\"))"
      symbol)))))

(defun lispy-flatten--lisp ()
  (let* ((bnd (lispy--bounds-list))
         (str (lispy--string-dwim bnd))
         (expr (read str))
         (fexpr (read (lispy--eval-lisp
                       (format "(function-lambda-expression #'%S)" (car expr))))))
    (if (not (eq (car-safe fexpr) 'SB-INT:NAMED-LAMBDA))
        (error "Could not find the body of %S" (car expr))
      (setq fexpr (downcase
                   (prin1-to-string
                    `(lambda ,(nth 2 fexpr) ,(cl-caddr (nth 3 fexpr))))))
      (goto-char (car bnd))
      (delete-region (car bnd) (cdr bnd))
      (let* ((e-args (cdr expr))
             (body (lispy--flatten-function fexpr e-args)))
        (lispy--insert body)))))

(defun lispy-goto-symbol-lisp (symbol)
  ;; start SLY or SLIME if necessary
  (lispy--cl-process)
  (if (lispy--use-sly-p)
      (sly-edit-definition symbol)
    (slime-edit-definition symbol)))

(provide 'le-lisp)

;;; le-lisp.el ends here
