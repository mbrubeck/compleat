Compleat
========

Generate tab completion for any shell command by specifying its usage in a
familiar manpage-like format.  For example, a usage specification for
`top(1)`:

    top [-b | -c | -H | -i | -s | -S | -d <delay> | -n <num> | -p <pid> ...] ... ;
    top (-h|-v)

The only supported shells at the moment are `bash` and `zsh`.

Installation
------------

Get the source code: `git clone git://github.com/mbrubeck/compleat.git`

Next, install GHC and Parsec.  OS X or Windows users, download the [Haskell
Platform][1].  (Mac OS X 10.6 may require a [workaround][2] for 64-bit
compatibility.) Debian/Ubuntu users, run: `sudo aptitude install
libghc6-parsec2-dev`

[1]: http://hackage.haskell.org/platform/
[2]: http://www.haskell.org/haskellwiki/Mac_OS_X

To install Compleat in your system, run: 

    ./Setup.lhs configure
    ./Setup.lhs build
    sudo ./Setup.lhs install

(This will install to `/usr/local` by default.   The "configure" command takes
a `--prefix=PATH` option to change the location, and `--user` to install as a
non-root user.)

### bash

To enable compleat in bash, add the following line to your `.bashrc`.
(Adjust the path if you configured with a custom prefix.)

    source /usr/local/share/compleat-1.0/compleat_setup

and install your .usage files in a directory named `/etc/compleat.d` or
`~/.compleat`:

    sudo mkdir /etc/compleat.d
    sudo cp examples/* /etc/compleat.d

Restart your shell to begin using completions:

    exec bash

### zsh

zsh support requires zsh >= 4.2.1, and currently uses zsh's bash-compatibility
mode rather than taking advantage of zsh's extended completion features.

To enable compleat in zsh, make the following change to your `.zshrc`.
(Adjust the path if you configured with a custom prefix.)

If you used the zsh wizard (zsh-newuser-install) to set up your `zshrc`, it should contain lines
like the following (if they don't exist, simply add the lines in the change below):

    autoload -Uz compinit
    compinit

Change these to:

    autoload -Uz compinit bashcompinit
    compinit
    bashcompinit

    source /usr/local/share/compleat-1.0/compleat_setup

and install your .usage files in a directory named `/etc/compleat.d` or
`~/.compleat`:

    sudo mkdir /etc/compleat.d
    sudo cp examples/* /etc/compleat.d

Restart your shell to begin using completions:

    exec zsh

### Testing

Type `top` and then press Tab a few times to see the example files in action.

Syntax
------

A usage file contains commands and definitions, separated by semicolons.

A *command* consists of a *command name* followed by a *pattern*.  The command
name can be any atom.  If there is more than one command in the file, compleat
will attempt to match each of them against the input line.

An *atom* consists of letters, numbers, and any of the characters `-_/@=+.,:`,
or any string enclosed in double quotes with C/Java-style backslash escapes.

The following are valid patterns:

* Any atom matches itself: `foo` matches the string `foo`.  `"x\\y"` matches
  the string `x\y`.
* `a b` matches `a` followed by `b`.
* `a b | c` matches either `a b` or `c`.
* `[a]` matches zero or one occurrences of `a`.
* `a ...` matches one or more occurrences of `a`.
* `[a] ...` matches zero or more occurrences of `a`.

Use parentheses to group patterns:

* `a (b | c)` matches `a` followed by either `b` or `c`.
* `(a | b) ...` matches `a` or `b` followed by any number of additional
  `a` or `b`.

Patterns may also include *variables*:

* `name = value;` defines a new variable.  The name can be any atom,
  and the value can be any pattern.  Then `<name>` in a pattern refers to the
  value as a sub-pattern.

* `name = !command;` defines a variable that uses a shell command to
  generate suggested completions.  The shell command should print one
  suggested completion per line.  The `$COMP_LINE` and `$COMP_CWORD`
  environment will contain the input line and the current word being
  completed.

* If no value is defined for `name`, then the pattern `<name>` will match any
  word.

Copyright
---------

Copyright (c) 2009 Matt Brubeck

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
