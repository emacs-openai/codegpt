[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/codegpt.svg)](https://jcs-emacs.github.io/jcs-elpa/#/codegpt)

<a href="#"><img align="right" src="./etc/logo.png" width="15%"></a>
# codegpt
> Use GPT-3 inside Emacs

[![CI](https://github.com/emacs-openai/codegpt/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-openai/codegpt/actions/workflows/test.yml)

This Emacs Code extension allows you to use the official OpenAI API to generate
code or natural language responses from OpenAI's [GPT-3](https://en.wikipedia.org/wiki/GPT-3)
to your questions, right within the editor.

<p align="center">
<img alt="explain" src="./etc/explain.png"/>
</p>

## 💾 Installation

#### package.el

This package is available from [JCS-ELPA](https://jcs-emacs.github.io/jcs-elpa/).
Install from these repositories then you should be good to go!

Normally, you don't need to add `(require 'codegpt)` to your configuration since
most `codegpt` commands are autoload and can be called without loading the module!

#### use-package

If you are using [use-package](https://www.emacswiki.org/emacs/UsePackage),
add the following to your `init.el` file:

```elisp
(use-package codegpt :ensure t)
```

or with `straight.el`:

```elisp
(use-package codegpt
  :straight (codegpt :type git :host github :repo "emacs-openai/codegpt"))
```

#### Manual installation

Copy all `.el` files in this repository to `~/.emacs.d/lisp` and add the following:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'codegpt)
```

## 🔑 Obtaining API key

To use this extension, you will need an API key from OpenAI. To obtain one,
follow these steps:

1. Go to [OpenAI's website](https://beta.openai.com/account/api-keys). If you
don't have an account, you will need to create one or sign up using your Google
or Microsoft account.
2. Click on the `Create new secret key` button.
3. Copy the key and paste into the 'API Key' field under the 'openai' custom group settings.

When you create a new account, you receive $18 in free credits for the API which
you must use in the first 90 days. You can see pricing information
[here](https://openai.com/api/pricing/). 1000 tokens are about 700 words, and
you can see the token count for each request at the end of the response in the
sidebar.

## 🔨 Usage

Highlight or select code using the `set-mark-command`, then do:

```
M-x codegpt
```

List of supported commands,

| Commad            | Description                                     |
|-------------------|-------------------------------------------------|
| `codegpt`         | The master command                              |
| `codegpt-custom`  | Write your own instruction                      |
| `codegpt-doc`     | Automatically write documentation for your code |
| `codegpt-fix`     | Find problems with it                           |
| `codegpt-explain` | Explain the selected code                       |
| `codegpt-improve` | Improve, refactor or optimize it                |

## 🌟 Using ChatGPT

The default is completing through the [Completions](https://platform.openai.com/docs/api-reference/completions)
tunnel. If you want to use ChatGPT, do the following:

```elisp
(setq codegpt-tunnel 'chat            ; The default is 'completion
      codegpt-model "gpt-3.5-turbo")  ; You can pick any model you want!
```

## 📝 Customization

#### 🧪 Variables

- `codegpt-tunnel`- Completion channel you want to use. (Default: `completion`)
- `codegpt-model` - ID of the model to use.
- `codegpt-max-tokens` - The maximum number of tokens to generate in the completion.
- `codegpt-temperature` - What sampling temperature to use.

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
