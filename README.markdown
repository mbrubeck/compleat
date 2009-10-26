Compleat
========

Generate tab completion for any shell command by specifying its usage in a
familiar manpage-like format.  For example, a usage specification for
`top(1)`:

    top [-b | -c | -H | -i | -s | -S | -d <delay> | -n <num> | -p <pid> ...] ... ;
    top (-h|-v)

The only supported shell at the moment is `bash`.

Instructions
============

1. Install GHC and Parsec.  OS X or Windows users, download the [Haskell
   Platform][1].  Debian/Ubuntu users, run: `sudo aptitude install
   libghc6-parsec3-dev`

2. Build the main program: `ghc --make compleat`

3. Try out the example completer: `complete -C "$PWD/compleat examples/top.usage"
   -o nospace top`

4. Type `top` and press "tab" to see the results.

[1]: http://hackage.haskell.org/platform/

Syntax
======

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
* `[a]` matches zero or more occurrences of `a`.
* `a ...` matches one or more occurrences of `a`.
* `[a] ...` matches zero or more occurrences of `a`.

Use parentheses to group patterns:

* `a (b | c)` matches `a` followed by either `b` or `c`.
* `(a | b) ...` matches `a` or `b` followed by any number of additional
  `a` or `b`.

Patterns may also include *variables*:

* `name = value;` defines a new variable.  The name can be any atom, and the
  value can be any pattern.
* `<name>` in a pattern will be replaced by the value of the `name` variable.
  If no value is defined, `<name>` will match any word.

Copyright
=========

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
