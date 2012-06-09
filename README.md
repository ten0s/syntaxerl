SyntaxErl
=========

SyntaxErl is a syntax checker tool for Erlang. The syntax checker currently supports erlang source files (.erl), erlang header files (.hrl), and erlang terms (.config, ...). Its main purpose is to be used in tools like Emacs's flymake http://www.emacswiki.org/emacs/FlymakeErlang and Vim's syntastic https://github.com/scrooloose/syntastic.

Building
--------

Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

### Dependencies

To build SyntaxErl you will need a working installation of Erlang, git, and GNU make.

#### Building SyntaxErl

$ git clone git://github.com/ten0s/syntaxerl.git
$ cd syntaxerl
$ make

After performing the steps above in the current working directory you now
have a script called `syntaxerl'. Place this script anywhere in your path.

Usage
-----

### Command line

$ syntaxerl
Usage: syntaxerl [-d] filename
Syntax checker for Erlang (0.0.1)

  -d, --debug    Enable debug output

### Emacs

### Vim

