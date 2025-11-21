;; -*- mode: eask; lexical-binding: t -*-

(package "codegpt"
         "0.1.0"
         "Use GPT-3 tp help you write code")

(website-url "https://github.com/emacs-openai/codegpt")
(keywords "convenience" "codegpt")

(package-file "codegpt.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)
(source 'jcs-elpa)

(depends-on "emacs" "28.1")
(depends-on "openai")
(depends-on "markdown-mode")
(depends-on "spinner")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
