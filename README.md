CL-STRING-MATCH [![Quickdocs](http://quickdocs.org/badge/cl-string-match.svg)](http://quickdocs.org/cl-string-match/) aims at providing robust implementations of string
matching algorithms. These algorithms are also called "[substring
search](http://en.wikipedia.org/wiki/String_searching_algorithm)"
or "subsequence search" algorithms.

Currently it provides implementations of the following string matching
algorithms (see [wiki for details](https://bitbucket.org/vityok/cl-string-match/wiki/Manual)):

* Brute-force (also known as naïve algorithm)
* Boyer-Moore (with mismatched character heuristic and good suffix shift)
* Boyer-Moore-Horspool algorithm
* Knuth-Morris-Pratt algorithm
* Rabin-Karp algorithm
* Shift-OR algorithm (single pattern)
* Aho-Corasick algorithm (with finite set of patterns, forward
  transition and fail function)
* A simple backtracking regular expressions engine
  [re](https://github.com/massung/re) similar to that of the Lua
  programming language. At the moment it significantly underperforms
  compared to the CL-PPCRE.

Some string processing algorithms are also implemented:

* Simple (naїve) suffix tree construction algorithm
* Ukkonen's suffix tree construction algorithm

Data structures:

* Prefix trie
* Suffix tree

Utilities:

* Testing whether a string has the given suffix or prefix (starts with
  or ends with the pattern)

Some algorithms (Brute-force, Boyer-Moore-Horspool) have parametric
implementations (code templates) making it possible to declare
specific implementations for application-specific custom data types
and data structures.

This library is routinely tested on Steel Bank CL, Clozure CL,
Embeddable CL and Armed Bear CL. Chances are really high that it will
work on other platforms without problems (check its status on
[CL-TEST-GRID](https://common-lisp.net/project/cl-test-grid/library/cl-string-match.html)).

Check the [API Reference](http://quickdocs.org/cl-string-match/) for more details.

Additional resources:

* [Project home page](https://bitbucket.org/vityok/cl-string-match)
* Also take a look at the [project Wiki](https://bitbucket.org/vityok/cl-string-match/wiki/Home)
* [A mirror on SourceForge](http://clstringmatch.sourceforge.net/)


RATIONALE
=========

Since the standard `search` function is working fine, one might ask:
why do we need a yet another implementation? Answer is simple:
advanced algorithms offer different benefits compared to the standard
implementation that is based on the brute-force algorithm.

[Benchmarks](https://bitbucket.org/vityok/cl-string-match/wiki/Benchmarks)
show that depending on environment and pattern of application, a
Boyer-Moore-Horspool algorithm implementation can outperform standard
search function in SBCL by almost 18 times! Check the code in the
`bench` folder for further details.


USAGE
=====

CL-STRING-MATCH [![Quickdocs](http://quickdocs.org/badge/cl-string-match.svg)](http://quickdocs.org/cl-string-match/) is supported by Quicklisp and is known by its system name:

```lisp
(ql:quickload :cl-string-match)
```

CL-STRING-MATCH exports functions in `cl-string-match` package (that
is also nicknamed as `sm`).

Shortcut functions search given pattern `pat` in text `txt`. They are
usually much slower (because they build index structures every time
they are called) but are easier to use:

* `string-contains-brute` *pat* *txt* — Brute-force
* `string-contains-bm` *pat* *txt* — Boyer-Moore
* `string-contains-bmh` *pat* *txt* — Boyer-Moore-Horspool
* `string-contains-kmp` *pat* *txt* — Knuth-Morris-Pratt
* `string-contains-ac` *pat* *txt* — Aho-Corasick
* `string-contains-rk` *pat* *txt* — Rabin-Karp

A more robust approach is to use pre-calculated index data that is
processed by a pair of `initialize` and `search` functions:

* `initialize-bm` *pat* and `search-bm` *bm* *txt*
* `initialize-bmh` *pat* and `search-bmh` *bm* *txt*
* `initialize-bmh8` *pat* and `search-bmh8` *bm* *txt*
* `initialize-rk` *pat* and `search-rk` *rk* *txt*
* `initialize-kmp` *pat* and `search-kmp` *kmp* *txt*
* `initialize-ac` *pat* and `search-ac` *ac* *txt*. `initialize-ac`
  can accept a list of patterns that are compiled into a trie.

Brute-force algorithm does not use pre-calculated data and has no
"initialize" function.

Boyer-Moore-Horspool implementation (the `-BMH` and `-BMH8` functions)
also accepts `:start2` and `:end2` keywords for the "search" and
"contains" functions.

Following example looks for a given substring *pat* in a given line of
text *txt* using Boyer-Moore-Horspool algorithm implementation:

```lisp
(let ((idx (initialize-bmh "abc")))
  (search-bmh idx "ababcfbgsldkj"))
```

Counting all matches of a given pattern in a string:

```lisp
(loop with str = "____abc____abc____ab"
      with pat = "abc"
      with idx = (sm:initialize-bmh8 pat)
      with z = 0 with s = 0 while s do
       (when (setf s (sm:search-bmh8 idx str :start2 s))
	 (incf z) (incf s (length pat)))
     finally (return z))
```

It should be noted that Boyer-Moore-Horspool (`bmh`) implementation
can offer an order of magnitude boost to performance compared to the
standard `search` function.

However, some implementations create a "jump table" that can be the
size of the alphabet (over 1M CHAR-CODE-LIMIT on implementations
supporting Unicode) and thus consume a significant chunk of
memory. There are different solutions to this problem and at the
moment a version for the ASCII strings is offered: `initialize-bmh8`
*pat* and `search-bmh8` *bm* *txt* as well as `string-contains-bmh8`
*pat* *txt* work for strings with characters inside the 256 char code
limit.

CONTRIB
=======

This project also contains code that is not directly invloved with the
pattern search algorithms but nevertheless might be found useful for
text handling/processing. Check the contrib folder in the repository
for more details. Currently it contains:

* `ascii-strings.lisp` aims to provide single-byte strings
functionality for Unicode-enabled Common Lisp implementations. Another
goal is to reduce memory footprint and boost performance of the
string-processing tasks, i.e. `read-line`.

* `simple-scanf` implements a subset of the original POSIX standard
`scanf(3)` function features.


TODO
====

The project still lacks some important features and is under constant
development. Any kind of contributions or feedback are welcome.

Please take a look at the [list of open issues](https://bitbucket.org/vityok/cl-string-match/issues?status=new&status=open) or the [Project Roadmap](https://bitbucket.org/vityok/cl-string-match/wiki/Project%20Roadmap).

Visit [project Wiki](https://bitbucket.org/vityok/cl-string-match/wiki/Home) for additional information.