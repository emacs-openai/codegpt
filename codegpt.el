;;; codegpt.el --- Use GPT-3 tp help you write code  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/codegpt
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (openai "0.1.0"))
;; Keywords: convenience codegpt

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Use GPT-3 tp help you write code
;;

;;; Code:

(require 'openai-completion)

(defgroup codegpt nil
  "Use GPT-3 tp help you write code."
  :prefix "codegpt-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/codegpt"))

(defun codegpt-code--internal (instruction start end)
  "Do INSTRUCTION with partial code.

The partial code is defined in with the region, and the START nad END are
boundaries of that region in buffer."
  (let ((text (string-trim (buffer-substring start end))))
    (openai-completon--ask-in-buffer
     instruction
     (insert text "\n\n")
     (openai-completion
      (buffer-string)
      (lambda (data)
        (openai--with-buffer openai-completion-buffer-name
          (openai--pop-to-buffer openai-completion-buffer-name)
          (let* ((choices (openai-completion--data-choices data))
                 (result (openai-completion--get-choice choices)))
            (insert (string-trim result) "\n"))))))))

;;;###autoload
(defun codegpt-doc (start end)
  "Automatically write documentation for your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (openai-completion-code--internal
   "Please write the documentation for the following function."
   start end))

;;;###autoload
(defun codegpt-fix (start end)
  "Fix your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (openai-completion-code--internal
   "There is a bug in the following function, please help me fix it."
   start end))

;;;###autoload
(defun codegpt-explain (start end)
  "Explain the selected code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (openai-completion-code--internal
   "What is the following?"
   start end))

;;;###autoload
(defun codegpt-improve (start end)
  "Improve, refactor or optimize your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (openai-completion-code--internal
   "Please improve the following."
   start end))

;;;###autoload
(defun codegpt-custom (start end)
  "Do completion with custom instruction.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (openai-completion-code--internal
   (read-string "Instruction: ")
   start end))

(defconst codegpt-action-alist
  `(("custom"  . "Write your own instruction")
    ("doc"     . "Automatically write documentation for your code")
    ("fix"     . "Find problems with it")
    ("explain" . "Explain the selected code")
    ("improve" . "Improve, refactor or optimize it"))
  "Alist of code completion actions and its' description.")

;;;###autoload
(defun codegpt (start end)
  "Do completon with OpenAI to your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (let*
      ((offset (openai--completing-frame-offset openai-completion-code-action-alist))
       (action
        (completing-read
         "Select completion action: "
         (lambda (string predicate action)
           (if (eq action 'metadata)
               `(metadata
                 (display-sort-function . ,#'identity)
                 (annotation-function
                  . ,(lambda (cand)
                       (concat (propertize " " 'display `((space :align-to (- right ,offset))))
                               (cdr (assoc cand openai-completion-code-action-alist))))))
             (complete-with-action action openai-completion-code-action-alist string predicate)))
         nil t)))
    (funcall
     (pcase action
       ("custom"  #'codegpt-custom)
       ("doc"     #'codegpt-doc)
       ("fix"     #'codegpt-fix)
       ("explain" #'codegpt-explain)
       ("improve" #'codegpt-improve))
     start end)))

(provide 'codegpt)
;;; codegpt.el ends here
