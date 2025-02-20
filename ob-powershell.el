;;; ob-powershell.el --- org-babel functions for powershell evaluation


;; Copyright (C) 2022 Rob Kiggen

;; Authors: Rob Kiggen <robby.kiggen@essential-it.be>
;;          Chris Bilson
;;          Pedro Morais <fpvmorais@gmail.com>
;; Maintainer: Mois Moshev <mois.moshev@bottleshipvfx.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: powershell, shell, execute, outlines, processes
;; URL: https://github.com/rkiggen/ob-powershell

;;; Commentary:

;; Org-Babel support for evaluating powershell source code.


;;; Code:
(require 'ob)
(require 'ob-core)
(eval-when-compile (require 'cl-lib))

(declare-function org-babel--get-vars "ob" (params))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("powershell" . "ps1"))

(defvar org-babel-default-header-args:powershell '())

(defcustom org-babel-powershell-os-command "powershell"
  "Shell command of the OS to use with `org-babel-execute:powershell'.
Examples:
- \"powershell\"
- \"pwsh\"
- \"pwsh-preview\"."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-powershell-hline-to "None"
  "Replace hlines in incoming tables with this when translating to powershell."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defun org-babel-expand-body:powershell (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
	(print-level nil)
	(print-length nil))
    (if (null vars) (concat body "\n")
      (format "%s\n%s\n"
	      (mapconcat
	       (lambda (var)
		 (format "$%s = %s" (car var) (org-babel-powershell-var-to-powershell (cdr var))))
	       vars "\n")
	      body))))

(defun org-babel-powershell-var-to-powershell (var)
  "Convert an elisp value to a powershell variable.
Convert an elisp value, VAR, into a string of powershell source code
specifying a variable of the same value."
  (if (listp var)
      (concat "@(" (mapconcat #'org-babel-powershell-var-to-powershell var ", ") ")")
    (if (eq var 'hline)
	org-babel-powershell-hline-to
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "\"\"%S\"\"" "%S")
       (if (stringp var) (substring-no-properties var) var)))))


(defun org-babel-execute:powershell (body params)
  "Execute a block of Powershell code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((in-file (org-babel-temp-file "powershell-" ".ps1"))
         (cmdline (or (cdr (assq :cmdline params))
                      "-NoProfile -NoLogo -NonInteractive"))
         (cmd (or (cdr (assq :cmd params))
                  org-babel-powershell-os-command)))
    (with-temp-file in-file
      (insert (org-babel-expand-body:powershell body params)))
    (message "cmd: %s" cmd)
    (message "cmdline: %s" cmdline)
    (message "in-file: %s" in-file)
    (message "body: %s" (org-babel-expand-body:powershell body params))
    (org-babel-eval
     (concat cmd " " cmdline
             " -File " (org-babel-process-file-name in-file))
     "")))

;; TODO: I think I *can* support sessions in powershell and really want to...
(defun org-babel-prep-session:powershell (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (error "Sessions are not supported for Powershell"))

(defun org-babel-variable-assignments:powershell (params)
  "Return list of powershell statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (org-babel-powershell--var-to-powershell (cdr pair) (car pair)))
   (mapcar #'cdr (org-babel--get-vars params))))

;; helper functions

(defvar org-babel-powershell--lvl 0)

(defun org-babel-powershell--var-to-powershell (var &optional varn)
  "Convert an elisp value to a powershell variable.
The elisp value, VAR, is converted to a string of powershell source code
specifying a var of the same value."
  (if varn
      (let ((org-babel-powershell--lvl 0) (lvar (listp var)) prefix)
        (concat "$" (symbol-name varn) "=" (when lvar "\n")
                (org-babel-powershell--var-to-powershell var)
                ";\n"))
    (let ((prefix (make-string (* 2 org-babel-powershell--lvl) ?\ )))
      (concat prefix
              (if (listp var)
                  (let ((org-babel-powershell--lvl (1+ org-babel-powershell--lvl)))
                    (concat "[\n"
                            (mapconcat #'org-babel-powershell--var-to-powershell var "")
                            prefix "]"))
                (format "${%s}" var))
              (unless (zerop org-babel-powershell--lvl) ",\n")))))

(defvar org-babel-powershell-buffers '(:default . nil))

(defun org-babel-powershell-initiate-session (&optional session params)
  "Return nil because sessions are not supported by powershell."
  nil)

(defvar org-babel-powershell-preface nil)

(defun org-babel-powershell-evaluate (session ibody &optional result-type result-params)
  "Pass BODY to the Powershell process in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for Powershell"))
  (let* ((body (concat org-babel-powershell-preface ibody))
         (out-file (org-babel-temp-file "powershell-"))
         (tmp-babel-file (org-babel-process-file-name
                          out-file 'noquote))
         (in-file (org-babel-temp-file "powershell-"))
         (command (format "%s -File '%s'" org-babel-powershell-command in-file)))

    (with-temp-file in-file
      (insert body))

    (message "body: %s" body)
    (message "in-file: %s" in-file)
    (message "out-file: %s" out-file)
    (message "command: %s" command)

    (let ((results
           (case result-type
             (output
              (with-temp-file out-file
                (insert
                 (org-babel-eval org-babel-powershell-command body))
                (buffer-string)))
             (value
              (message "evaliing now...")
              (org-babel-eval command body)))))
      (when results
        (org-babel-result-cond result-params
          (org-babel-eval-read-file out-file)
          (org-babel-import-elisp-from-file out-file '(16)))))))

(provide 'ob-powershell)
;;; ob-powershell.el ends here
