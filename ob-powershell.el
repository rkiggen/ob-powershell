;;; Commentary:

;; Currently this only supports the external compilation and execution
;; of Powershell code blocks (i.e., no session support).
;;; Code:

(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("powershell" . "ps1"))

(defun org-babel-powershell-powershell-command "powershell"
  "Name of command used to evaluate powershell blocks."
  :group 'org-babel
  :version "24.3"
  :type 'string)


(defun org-babel-execute:powershell (body params)
  "Execute a block of Powershell code with Babel.
  This function is called by `org-babel-execute-src-block'."
  (setq scriptfile (org-babel-temp-file "powershell-script-" ".ps1"))
  (setq full-body (org-babel-expand-body:generic
		   body params (org-babel-variable-assignments:powershell)))
  (with-temp-file scriptfile (insert full-body)))


  (defun org-babel-variable-assignments:powershell (params)
    "Return a list of Powershell statements assigning the block's variables."
    (mapcar
     (lambda (pair)
       (format "$%s=%s"
               (car pair)
               (org-babel-powershell-var-to-powershell (cdr pair))))
     (mapcar #'cdr (org-babel-get-header params :var))))

  (defun org-babel-powershell-var-to-powershell (var)
    "Convert :var into a powershell variable.
             Convert an elisp value, VAR, into a string of poershell source code
             specifying a variable of the same value."
    (if (listp var)
        (concat "[" (mapconcat #'org-babel-powershell-var-to-powershell var ", ") "]")
      (format "$%S" var)))

  (defun org-babel-prep-session:powershell (session params)
    "Return an error because Powershell does not support sessions."
    (error "Sessions are not (yet) supported for Powershell"))


(provide 'ob-powershell)
;;; ob-powershell.el ends here
