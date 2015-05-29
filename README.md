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
* Rabin-Karp algorithm
* Knuth-Morris-Pratt algorithm
* Aho-Corasick algorithm (with finite set of patterns)

Some string processing algorithms are also implemented:

* Simple (naїve) suffix tree construction algorithm
* Ukkonen's suffix tree construction algorithm

Data structures:

* Prefix trie
* Suffix tree

Resources:

* [Project home page](http://sourceforge.net/projects/clstringmatch/)

* Also take a look at the [project Wiki](http://sourceforge.net/p/clstringmatch/wiki/Home/)

* If you prefer other project hosting sites, take a look at [our mirror on BitBucket](http://bitbucket.org/vityok/cl-string-match)


TODO
====

The project still lacks some important features and is under
development. Following tasks are still to be implemented:

* Comprehensive unit test suite: test if the functions in this package
  work properly

* Better replication of the SEARCH function parameters, implement
  search on generic sequences where possible, not just on
  strings. Search on byte sequences/arrays should be possible.

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
