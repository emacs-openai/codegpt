;;; codegpt.el --- Use GPT-3 tp help you write code  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/codegpt
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (openai "0.1.0") (spinner "1.7.4"))
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

(require 'cl-lib)

(require 'openai)
(require 'openai-chat)
(require 'openai-completion)
(require 'spinner)

(defgroup codegpt nil
  "Use GPT-3 tp help you write code."
  :prefix "codegpt-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/codegpt"))

(defcustom codegpt-focus-p t
  "If this value is `nil`, do not move focus to output buffer."
  :type 'boolean
  :group 'codegpt)

(defconst codegpt-buffer-name "*CodeGPT*"
  "Buffer name to do completion task.")

(defcustom codegpt-action-alist
  `(("custom"  . "Write your own instruction")
    ("doc"     . "Automatically write documentation for your code")
    ("fix"     . "Find problems with it")
    ("explain" . "Explain the selected code")
    ("improve" . "Improve, refactor or optimize it"))
  "Alist of code completion actions and its' description."
  :type 'list
  :group 'codegpt)

(defcustom codegpt-tunnel 'completion
  "Tunnel to use for the tasks."
  :type '(choice (const :tag "Through Completion" completion)
                 (const :tag "Through ChatGPT" chat))
  :group 'codegpt)

(defcustom codegpt-model "text-davinci-003"
  "ID of the model to use."
  :type 'string
  :group 'codegpt)

(defcustom codegpt-max-tokens 4000
  "The maximum number of tokens to generate in the completion."
  :type 'integer
  :group 'codegpt)

(defcustom codegpt-temperature 1.0
  "What sampling temperature to use."
  :type 'number
  :group 'openai)

(defcustom codegpt-spinner-type 'moon
  "The type of the spinner."
  :type '(choice (const :tag "Key to variable `spinner-types'" symbol)
                 (const :tag "Vector of characters" vector))
  :group 'openai)

(defvar codegpt-requesting-p nil
  "Non-nil if sitll requesting.")

(defvar codegpt-spinner-counter 0
  "Spinner counter.")

(defvar codegpt-spinner-timer nil
  "Spinner timer.")

;;
;;; Major Mode

(defun codegpt-header-line ()
  "Header line for CodeGPT."
  (format " %s[Tunnel] %s  [Model] %s"
          (if codegpt-requesting-p
              (let* ((spinner (if (symbolp codegpt-spinner-type)
                                  (cdr (assoc codegpt-spinner-type spinner-types))
                                codegpt-spinner-type))
                     (len (length spinner)))
                (when (<= len codegpt-spinner-counter)
                  (setq codegpt-spinner-counter 0))
                (format "%s " (elt spinner codegpt-spinner-counter)))
            "")
          codegpt-tunnel codegpt-model))

(defun codegpt-mode--cancel-timer ()
  "Cancel spinner timer."
  (when (timerp codegpt-spinner-timer)
    (cancel-timer codegpt-spinner-timer)))

;;;###autoload
(define-derived-mode codegpt-mode fundamental-mode "CodeGPT"
  "Major mode for `codegpt-mode'.

\\<codegpt-mode-map>"
  (setq codegpt-spinner-counter 0)
  (setq-local header-line-format `((:eval (codegpt-header-line))))
  (add-hook 'kill-buffer-hook #'codegpt-mode--cancel-timer nil t)
  (codegpt-mode--cancel-timer)
  (setq codegpt-spinner-timer (run-with-timer (/ spinner-frames-per-second 60.0)
                                              (/ spinner-frames-per-second 60.0)
                                              (lambda ()
                                                (cl-incf codegpt-spinner-counter)
                                                (force-mode-line-update)))))

;;
;;; Application

(defmacro codegpt--ask-in-buffer (instruction &rest body)
  "Insert INSTRUCTION then execute BODY form."
  (declare (indent 1))
  `(progn
     (openai--pop-to-buffer codegpt-buffer-name)  ; create it
     (openai--with-buffer codegpt-buffer-name
       (codegpt-mode)
       (erase-buffer)
       (insert ,instruction "\n\n")
       ,@body)))

(defun codegpt--fill-region (start end)
  "Like function `fill-region' (START to END), improve readability."
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (when (< fill-column (current-column))
        (fill-region (line-beginning-position) (line-end-position)))
      (forward-line 1))))

(defun codegpt--internal (instruction start end)
  "Do INSTRUCTION with partial code.

The partial code is defined in with the region, and the START nad END are
boundaries of that region in buffer."
  (setq codegpt-requesting-p t)
  (let ((text (string-trim (buffer-substring start end)))
        (original-window (selected-window)))
    (codegpt--ask-in-buffer instruction
      (insert text "\n\n")
      (funcall
       (cl-case codegpt-tunnel
         (`completion #'openai-completion)
         (`chat       #'openai-chat))
       (cl-case codegpt-tunnel
         (`completion (buffer-string))
         (`chat       `[(("role"    . "user")
                         ("content" . ,(buffer-string)))]))
       (lambda (data)
         (setq codegpt-requesting-p nil)
         (codegpt-mode--cancel-timer)
         (openai--with-buffer codegpt-buffer-name
           (openai--pop-to-buffer codegpt-buffer-name)
           (let ((original-point (point)))
             (cl-case codegpt-tunnel
               (`completion
                (let* ((choices (openai--data-choices data))
                       (result (openai--get-choice choices)))
                  (insert (string-trim result) "\n")))
               (`chat
                (let ((choices (let-alist data .choices))
                      (result))
                  (mapc (lambda (choice)
                          (let-alist choice
                            (let-alist .message
                              (setq result (string-trim .content)))))
                        choices)
                  (insert (string-trim result) "\n"))))
             (codegpt--fill-region original-point (point))))
         (unless codegpt-focus-p
           (select-window original-window)))
       :model codegpt-model
       :max-tokens codegpt-max-tokens
       :temperature codegpt-temperature)
      (unless codegpt-focus-p
        (select-window original-window)))))

;;;###autoload
(defun codegpt-doc (start end)
  "Automatically write documentation for your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (codegpt--internal
   "Please write the documentation for the following function."
   start end))

;;;###autoload
(defun codegpt-fix (start end)
  "Fix your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (codegpt--internal
   "There is a bug in the following function, please help me fix it."
   start end))

;;;###autoload
(defun codegpt-explain (start end)
  "Explain the selected code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (codegpt--internal
   "What is the following?"
   start end))

;;;###autoload
(defun codegpt-improve (start end)
  "Improve, refactor or optimize your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (codegpt--internal
   "Please improve the following."
   start end))

;;;###autoload
(defun codegpt-custom (start end)
  "Do completion with custom instruction.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (codegpt--internal
   (read-string "Instruction: ")
   start end))

(defun codegept--execute-predefined-template (start end question)
  "Ask predefined QUESTION for provided region.
the START and END are boundaries of that region in buffer."
  (codegpt--internal question start end))

;;;###autoload
(defun codegpt (start end)
  "Do completion with OpenAI to your code.

This command is interactive region only, the START and END are boundaries of
that region in buffer."
  (interactive "r")
  (let*
      ((offset (openai--completing-frame-offset codegpt-action-alist))
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
                               (cdr (assoc cand codegpt-action-alist))))))
             (complete-with-action action codegpt-action-alist string predicate)))
         nil t))
       ;; XXX: Guess the function name, `codegpt-xxx'
       (action-fn (intern (format "codegpt-%s" action))))
    (if (fboundp action-fn)
        (funcall action-fn start end)
      ;; Call predefined action from `codegpt-action-alist'
      (codegept--execute-predefined-template
       start end
       (cdr (assoc action codegpt-action-alist))))))

(provide 'codegpt)
;;; codegpt.el ends here
