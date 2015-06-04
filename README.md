CL-STRING-MATCH aims at providing robust implementations of string
matching algorithms. These algorithms are also called "substring
search" or "subsequence search" algorithms.

Corresponding article on Wikipedia is:

http://en.wikipedia.org/wiki/String_searching_algorithm

Currently it provides implementations of the following string matching
algorithms:

* Brute-force (also known as naïve algorithm)
* Boyer-Moore (with mismatched character heuristic)
* Boyer-Moore-Horspool algorithm
* Knuth-Morris-Pratt algorithm
* Aho-Corasick algorithm (with finite set of patterns)

Some string processing algorithms are also implemented:

* Simple (naїve) suffix tree construction algorithm
* Ukkonen's suffix tree construction algorithm

Data structures:

* Prefix trie
* Suffix tree

Some algorithms (Brute-force, Boyer-Moore-Horspool) have parametric
implementations making it possible to declare specific implementations
for application-specific custom data types and data structures.

Additional resources:

* [Project home page](https://bitbucket.org/vityok/cl-string-match)
* Also take a look at the [project Wiki](http://sourceforge.net/p/clstringmatch/wiki/Home/)
* If you prefer other project hosting sites, take a look at [our mirror on SourceForge](http://clstringmatch.sourceforge.net/)


USAGE
=====

cl-string-match is supported by Quicklisp and is known by its system name:
```lisp
    (ql:quickload :cl-string-match)
```

Cl-string-match exports functions in `cl-string-match` package (nicknamed `sm`).

Shortcuts look for given pattern `pat` in text `txt`. They are usually much slower but are easier to use:

* `string-contains-brute` *pat* *txt* — Brute-force
* `string-contains-bm` *pat* *txt* — Boyer-Moore
* `string-contains-bmh` *pat* *txt* — Boyer-Moore-Horspool
* `string-contains-kmp` *pat* *txt* — Knuth-Morris-Pratt
* `string-contains-ac` *pat* *txt* — Aho-Corasick

A more robust approach is to use pre-calculated index data that is
processed by a pair of `initialize` and `search` functions:

* `initialize-bm` *pat* and `search-bm` *bm* *txt*
* `initialize-bmh` *pat* and `search-bm` *bm* *txt*
* `initialize-rk` *pat* and `search-rk` *rk* *txt*
* `initialize-kmp` *pat* and `search-kmp` *kmp* *txt*
* `initialize-ac` *pat* and `search-ac` *ac* *txt*. `initialize-ac`
  can accept a list of patterns that are compiled into a trie.

Brute-force algorithm does not use pre-calculated data and has no
"initialize" function.

Following example looks for a given substring *pat* in a given line of
text *txt* using Boyer-Moore-Horspool algorithm implementation:
```lisp
    (let ((idx (initialize-bmh "abc")))
      (search-bmh idx "ababcfbgsldkj"))
```

It should be noted that Boyer-Moore-Horspool (`bmh`) implementation
offers an order of magnitude boost to performance compared to the
standard `search` function.

TODO
====

The project still lacks some important features and is under
development. Following tasks are still to be implemented:

* Comprehensive unit test suite: test if the functions in this package
  work properly

* Better replication of the SEARCH function parameters, implement
  search on generic sequences where possible, not just on
  strings. Search on byte sequences/arrays should be possible.

* Rabin-Karp algorithm is not implemented properly: type conversions
  and native arithmetic operations bloat code and make it barely
  readable. Function returns incorrect results in some cases.

* Improve performance: some implementations (i.e. Aho-Corasick,
  Rabin-Karp) are extremely slow compared with theoretical boundaries.

* Benchmark should include corner cases (general worst-case scenarios)
  and also check correlation between needle and haystack sizes

* It is possible to generate code for automata and compile it into
  native code using standard Lisp facilities [as described by Peter
  Seibel](http://gigamonkeys.wordpress.com/2007/07/27/compiling-queries-without-eval/)

* Utilize random haystack for benchmarking similar to CL-IRREGSEXP

* Finite-State automaton algorithm

* Apostolico–Giancarlo algorithm

Algorithms with finite set of patterns:

* Commentz-Walter algorithm

* Rabin–Karp string search algorithm

* DAWG-match

Additional algorithms:

* Suffix tree and tree-based algorithms

* Thompson NFA

* Consider inexact matching and other algorithms on strings
