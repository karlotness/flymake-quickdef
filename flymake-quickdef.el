;;; flymake-quickdef.el --- Quickly define a new Flymake backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Karl Otness

;; Author: Karl Otness
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, tools, convenience, lisp
;; URL: https://github.com/karlotness/flymake-quickdef

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; Flymake backend definition macro and support

(defvar-local flymake-quickdef--procs nil
  "Internal variable used by `flymake-quickdef-backend'.
Do not edit its value. This variable holds a plist used to store
handles to running processes for Flymake backends. Entries are
keyed by the symbol name of the appropriate backend function and
values are running processes.")

;;;###autoload
(defmacro flymake-quickdef-backend (name &optional docstring &rest plist)
  "Quickly define a backend for use with Flymake.
This macro produces a new function, NAME, which is suitable to
insert into `flymake-diagnostic-functions'. The function will
have documentation as specified in the mandatory DOCSTRING. The
body of the function will be generated based on the forms passed
in through DEF-PLIST as documented below.

The plist is keyed by several keyword arguments: \":pre-let\",
\":pre-check\", \":write-type\", \":proc-form\",
\":search-regexp\", and \":prep-diagnostic\".

The forms passed to this macro may also make use of several
special variables: \"source\" and \"temp-file\". The variable
\"source\" stores a reference to the buffer containing the text
to check with Flymake. It is initialized to `current-buffer' at
the beginning of evaluation. The variable \"temp-file\" is
defined when :write-type is 'file. It contains the path of the
temporary file which was created for the current backend run.

The implementation of this macro is derived from Flymake manual.
See info node `(flymake)An annotated example backend'."
  (declare (indent defun) (doc-string 2))
  (let* ((def-docstring (when (stringp docstring) docstring))
         (def-plist (if (stringp docstring) plist (cons docstring plist)))
         (write-type (or (eval (plist-get def-plist :write-type)) 'pipe))
         (temp-dir-symb (make-symbol "fmqd-temp-dir"))
         (fmqd-err-symb (make-symbol "fmqd-err"))
         (cleanup-form (when (eq write-type 'file)
                         (list (list 'delete-directory temp-dir-symb t)))))
    (dolist (elem '(:proc-form :search-regexp :prep-diagnostic))
      (unless (plist-get def-plist elem)
        (error "Missing flymake backend definition `%s'" elem)))
    (unless (memq (eval (plist-get def-plist :write-type)) '(file pipe nil))
      (error "Invalid `:write-type' value `%s'" (plist-get def-plist :write-type)))
    `(defun ,name (report-fn &rest _args)
       ,def-docstring
       (let* ((fmqd-source (current-buffer))
              ;; If storing to a file, create the temporary directory
              ,@(when (eq write-type 'file)
                  `((,temp-dir-symb (make-temp-file "flymake-" t))
                    (fmqd-temp-file
                     (concat
                      (file-name-as-directory ,temp-dir-symb)
                      (file-name-nondirectory (or (buffer-file-name) (buffer-name)))))))
              ;; Next we do the :pre-let phase
              ,@(plist-get def-plist :pre-let))
         ;; With vars defined, do :pre-check
         (condition-case ,fmqd-err-symb
             (progn
               ,(plist-get def-plist :pre-check))
           (error ,@cleanup-form
                  (signal (car ,fmqd-err-symb) (cdr ,fmqd-err-symb))))
         ;; No errors so far, kill any running (obsolete) running processes
         (let ((proc (plist-get flymake-quickdef--procs ',name)))
           (when (process-live-p proc)
             (kill-process proc)))
         (save-restriction
           (widen)
           ;; If writing to a file, send the data to the temp file
           ,@(when (eq write-type 'file)
               '((write-region nil nil fmqd-temp-file nil 'silent)))
           (setq flymake-quickdef--procs
                 (plist-put flymake-quickdef--procs ',name
                            (make-process
                             :name ,(concat (symbol-name name) "-flymake")
                             :noquery t
                             :connection-type 'pipe
                             :buffer (generate-new-buffer ,(concat " *" (symbol-name name) "-flymake*"))
                             :command ,(plist-get def-plist :proc-form)
                             :sentinel
                             (lambda (proc _event)
                               ;; If the process is actually done we can continue
                               (unless (process-live-p proc)
                                 (unwind-protect
                                     (if (eq proc (plist-get (buffer-local-value 'flymake-quickdef--procs fmqd-source) ',name))
                                         ;; This is the current process
                                         ;; Widen the code buffer so we can compute line numbers, etc.
                                         (with-current-buffer fmqd-source
                                           (save-restriction
                                             (widen)
                                             ;; Scan the process output for errors
                                             (with-current-buffer (process-buffer proc)
                                               (goto-char (point-min))
                                               (save-match-data
                                                 (let ((diags nil))
                                                   (while (search-forward-regexp
                                                           ,(plist-get def-plist :search-regexp)
                                                           nil t)
                                                     ;; Save match data to work around a bug in `flymake-diag-region'
                                                     ;; That function seems to alter match data and is commonly called here
                                                     (save-match-data
                                                       (save-excursion
                                                         (let ((d (apply 'flymake-make-diagnostic
                                                                         ,(plist-get def-plist :prep-diagnostic))))
                                                           ;; Skip any diagnostics with a type of nil
                                                           ;; This makes it easier to filter some out
                                                           (when (flymake-diagnostic-type d)
                                                             (push d diags))))))
                                                   (funcall report-fn (nreverse diags)))))))
                                       ;; Else case: this process is obsolete
                                       (flymake-log :warning "Canceling obsolete check %s" proc))
                                   ;; Unwind-protect cleanup forms
                                   ,@cleanup-form
                                   (kill-buffer (process-buffer proc))))))))
           ;; If piping, send data to process
           ,@(when (eq write-type 'pipe)
               `((let ((proc (plist-get flymake-quickdef--procs ',name)))
                   (process-send-region proc (point-min) (point-max))
                   (process-send-eof proc)))))))))

(provide 'flymake-quickdef)
;;; flymake-quickdef.el ends here
