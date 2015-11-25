[![Build Status](https://travis-ci.org/ten0s/syntaxerl.svg?branch=master)](https://travis-ci.org/ten0s/syntaxerl)

SyntaxErl
=========

SyntaxErl is a syntax checker tool for Erlang. The syntax checker currently
supports erlang source files (.erl), erlang header files (.hrl), erlang configs
(.config, .rel, .script, .app, .app.src), escript files (.erl, .escript, .es),
leex files (.xrl), and yecc files (.yrl). Its main purpose is to be used by tools
like Emacs's flymake http://www.emacswiki.org/emacs/FlymakeErlang and Vim's
syntastic https://github.com/scrooloose/syntastic.


SyntaxErl uses the [rebar](https://github.com/basho/rebar) config under the
hood to determine deps and libs paths. Some common compile options are
hardcoded, while others project specific are read from the rebar config file.
For the syntax checker to work correctly, make sure that your project is
compilable and all the deps are at their places.

Building
--------

Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

### Dependencies

To build SyntaxErl you will need a working installation of Erlang, git, and GNU make.

#### Building SyntaxErl

```sh
$ git clone git://github.com/ten0s/syntaxerl.git
$ cd syntaxerl
$ make
```

After performing the steps above in the current working directory you now
have a script called `syntaxerl'. Place this script anywhere in your path.

Usage
-----

### Command line

```sh
$ syntaxerl
Syntax checker for Erlang (0.10.0)
Usage: syntaxerl [-d | --debug] <FILENAME>
       syntaxerl <-h | --help>
  -d, --debug    Enable debug output
  -h, --help     Show this message
```

### Emacs

#### Flymake

```elisp
;;;----------------------------------------
;;; erlang-mode
;;;----------------------------------------

(setq erlang-root-dir "/opt/r16b03-1")
(setq load-path (cons (car (file-expand-wildcards (concat erlang-root-dir "/lib/tools-*/emacs"))) load-path))
(setq erlang-electric-commands nil)
(require 'erlang-start)

(add-hook 'erlang-mode-hook
  '(lambda()
     (imenu-add-to-menubar "Imenu")))

; define auto erlang mode for these files/extensions.
(add-to-list 'auto-mode-alist '(".*\\.app\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".*app\\.src\\'"  . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.config\\'"  . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.rel\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.script\\'"  . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.escript\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.es\\'"      . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.xrl\\'"     . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.yrl\\'"     . erlang-mode))

; add include directory to default compile path.
(defvar erlang-compile-extra-opts
  '(bin_opt_info debug_info (i . "../include") (i . "../deps") (i . "../../") (i . "../../../deps")))

; define where put beam files.
(setq erlang-compile-outdir "../ebin")

;;;----------------------------------------
;;; flymake
;;;----------------------------------------

(require 'flymake)
(require 'flymake-cursor) ; http://www.emacswiki.org/emacs/FlymakeCursor
(setq flymake-log-level 3)

(defun flymake-compile-script-path (path)
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list path (list local-file))))

(defun flymake-syntaxerl ()
  (flymake-compile-script-path "~/bin/syntaxerl"))

(add-hook 'erlang-mode-hook
  '(lambda()
     (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'"     flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.hrl\\'"     flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.xrl\\'"     flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.yrl\\'"     flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.app\\'"     flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.app.src\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.config\\'"  flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.rel\\'"     flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.script\\'"  flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.escript\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.es\\'"      flymake-syntaxerl))

     ;; should be the last.
     (flymake-mode 1)
))

; see /usr/local/lib/erlang/lib/tools-<Ver>/emacs/erlang-flymake.erl
(defun erlang-flymake-only-on-save ()
  "Trigger flymake only when the buffer is saved (disables syntax
check on newline and when there are no changes)."
  (interactive)
  ;; There doesn't seem to be a way of disabling this; set to the
  ;; largest int available as a workaround (most-positive-fixnum
  ;; equates to 8.5 years on my machine, so it ought to be enough ;-) )
  (setq flymake-no-changes-timeout most-positive-fixnum)
  (setq flymake-start-syntax-check-on-newline nil))

(erlang-flymake-only-on-save)
```

#### Erlang-flymake

Your help is welcome.

### Vim

#### Syntastic

Setup everything as described [here](https://github.com/scrooloose/syntastic).
Then add

    let g:syntastic_erlang_checkers=['syntaxerl']

to your vimrc.

Thanks [locojay](https://github.com/locojay) for that.
