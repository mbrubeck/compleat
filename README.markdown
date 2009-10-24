Compleat
========

Use Compleat to add command-line completion for any program by specifying its
usage in a familiar manpage-like format.  For example, a usage specification
for `top(1)`:

    top [-b | -c | -H | -i | -s | -S | -d <delay> | -n <num> | -p <pid> ...] ... ;
    top (-h|-v)

Compleat is a work in progress, and is not yet fully usable.  However, the
core functionality is ready to try out.  The only supported shell at the
moment is `bash`.

Instructions
============

1. Install GHC and Parsec.  OS X or Windows users, download the [Haskell
   Platform][1].  Debian/Ubuntu users, run: `sudo aptitude install
   libghc6-parsec3-dev`

2. Build the main program: `ghc --make Main`

3. Try out the example completer: `complete -C "$PWD/Main examples/top.usage"
   -o nospace top`

4. Type `top` and press "tab" to see the results.

[1]: http://hackage.haskell.org/platform/

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
