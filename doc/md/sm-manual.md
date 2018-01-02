# CL-STRING-MATCH Manual

## Table of Contents

- 1 cl-string-match ASDF System Details
- 2 Single pattern search
    - 2.1 Brute force
    - 2.2 Boyer-Moore algorithm
    - 2.3 Boyer-Moore-Horspool algorithm
    - 2.4 Rabin-Karp algorithm
    - 2.5 Knuth-Morris-Pratt algorithm
    - 2.6 Shift-OR algorithm
- 3 Multiple pattern search
    - 3.1 Aho-Corasick algorithm
- 4 Regular expressions
    - 4.1 Portable RE by Massung
        - 4.1.1 Compiling Patterns
        - 4.1.2 Basic Pattern Matching
        - 4.1.3 Pattern Scanning
        - 4.1.4 Splitting by Pattern
        - 4.1.5 Replacing by Pattern
        - 4.1.6 Groups
        - 4.1.7 The `with-re-match` Macro

###### \[in package CL-STRING-MATCH\]
`CL-STRING-MATCH` String and pattern matching library reference.

## 1 cl-string-match ASDF System Details

- Version: 2018.1.2
- Description: Provides implementations of the standard sub-string search (string
matching) algorithms: brute-force, Boyer-Moore, Rabin-Karp, etc.
- Licence: BSD
- Author: Vityok https://bitbucket.org/vityok
- Homepage: [https://bitbucket.org/vityok/cl-string-match](https://bitbucket.org/vityok/cl-string-match)

## 2 Single pattern search

Looking for a single pattern in a string

### 2.1 Brute force

A Brute-force algorithm is one of the simpliest but less robust among
the substring search algorithms.

`CL-STRING-MATCH` offers a code template for application specific
sequence and data types: `DEFINE-BRUTE-MATCHER` and two pre-defined
brute search functions, one for a standard Lisp
string (`STRING-CONTAINS-BRUTE`) and another for unsigned-byte (8 bits
per char) strings (`STRING-CONTAINS-BRUTE-UB`).

- [macro] **DEFINE-BRUTE-MATCHER** *VARIANT-TAG &KEY (KEY-GET 'CHAR) (KEY-CMP/= 'CHAR/=) (DATA-TYPE 'SIMPLE-STRING)*

- [function] **STRING-CONTAINS-BRUTE** 

    A Brute-force substring search implementation.
    
    Brute-force substring search requires O(N x M) character compares to
    search for a pattern of length M in a text of length N, in the worst
    case.
    
    Algorithm described in: Chapter 5, p. 760 in
      'Algorithms', Robert Sedgewick and Kevin Wayne. 4th

- [function] **STRING-CONTAINS-BRUTE-UB** 

    A Brute-force substring search implementation.
    
    Brute-force substring search requires O(N x M) character compares to
    search for a pattern of length M in a text of length N, in the worst
    case.
    
    Algorithm described in: Chapter 5, p. 760 in
      'Algorithms', Robert Sedgewick and Kevin Wayne. 4th

### 2.2 Boyer-Moore algorithm

The following implementation is based on algorithm described in:

Algorithm described in: Chapter 5, p. 772 in
 “Algorithms”, Robert Sedgewick and Kevin Wayne. 4th

\`\`Efficient Text Searching in Java'' By Laura Werner.
(appeared in Java Report, February 1999)

http://icu-project.org/docs/papers/efficient\_text\_searching\_in\_java.html

And some other sources.

Current implementation uses bad character and good suffix skip
heuristics.

- [function] **INITIALIZE-BM** 

- [function] **SEARCH-BM** 

    Search for pattern bm in txt.

- [function] **STRING-CONTAINS-BM** 

### 2.3 Boyer-Moore-Horspool algorithm

Boyer-Moore-Horspool algorithm is a simplification of the
Boyer-Moore algorithm;

- preprocessing phase in O(m+s) time and O(s) space complexity;

- searching phase in O(mn) time complexity;

- the average number of comparisons for one text character is between 1/s and 2/(s+1).

implementation based on the description from the book

"[Exact String Matching
Algorithms](http://www-igm.univ-mlv.fr/~lecroq/string/node18.html#SECTION00180)"
by Christian Charras and Thierry Lecroq

It uses only the "Bad character skip" rule, and does not use the
"Good suffix rule"

- [macro] **DEFINE-BMH-MATCHER** *VARIANT-TAG &KEY (KEY-GET 'CHAR) (KEY-CODE 'CHAR-CODE) (KEY-CMP= 'CHAR=) (EMPTY-PAT "") (ALPHABET-SIZE CHAR-CODE-LIMIT) (DATA-TYPE 'SIMPLE-STRING)*

- [function] **INITIALIZE-BMH8** 

    Preprocess the needle.
    Initialize the table to default value.

- [function] **SEARCH-BMH8** 

    Search for pattern defined in the `IDX` in `TXT`.

- [function] **STRING-CONTAINS-BMH8** 

### 2.4 Rabin-Karp algorithm

additional information can be found at:

http://www.geeksforgeeks.org/searching-for-patterns-set-3-rabin-karp-algorithm/

or:

http://www-igm.univ-mlv.fr/~lecroq/string/node5.html

Parts of the source code are modeled after the Java implementation
by Robert Sedgewick and Kevin Wayne:

http://algs4.cs.princeton.edu/53substring/RabinKarp.java.html

- [function] **INITIALIZE-RK** 

- [function] **SEARCH-RK** 

    Implementation of the Rabin-Karp substring search algorithm.

- [function] **STRING-CONTAINS-RK** 

### 2.5 Knuth-Morris-Pratt algorithm

The following implementation is based on algorithm described in:

Algorithm described in: Chapter 5, p. 768 in
 “Algorithms”, Robert Sedgewick and Kevin Wayne. 4th

Based on implementation by: “[Knuth-Morris-Pratt vs. Boyer–Moore in
LISP](http://www.ioremap.net/archive/other/lisp/optimized-bm-kmp-string-test.lisp).”
by ZBR

- [function] **INITIALIZE-KMP** 

- [function] **SEARCH-KMP** 

- [function] **STRING-CONTAINS-KMP** 

### 2.6 Shift-OR algorithm

Shift-OR single pattern search algorithm implementation.

Used

http://www-igm.univ-mlv.fr/~lecroq/string/node6.html#SECTION0060

As the blueprint for this implementation.

At the moment existing implementation of the Shift-OR search
algorithm is over 1.5 times slower than the standard `SEARCH`
function on SBCL and much more sluggish than the fast search
implementations offered by this library (`BMH` first of all).

- [function] **INITIALIZE-SOR** 

- [function] **SEARCH-SOR** 

- [function] **STRING-CONTAINS-SOR** 

## 3 Multiple pattern search

Looking for multiple patterns in a string

### 3.1 Aho-Corasick algorithm

Based on description in:

- Biosequence Algorithms, Spring 2005.  Lecture 4: [Set Matching and
Aho-Corasick
Algorithm](http://www.cs.uku.fi/~kilpelai/BSA05/lectures/slides04.pdf)
Pekka Kilpelainen. University of Kuopio, Department of Computer
Science


- [function] **EMPTY-TRIE** 

    Creates a new instance and returns an empty trie.

- [function] **TRIE-ADD-KEYWORD** 

    Starting at the root, follow the path labeled by chars of Pi
    
    If the path ends before Pi, continue it by adding new edges and nodes
    for the remaining characters of Pi.
    
    Store identifier i of Pi at the terminal node of the path.

- [function] **TRIE-CONTAINS** 

    Returns `T` if the given `TRIE` contains the given string S.

- [function] **TRIE-CONTAINS-PREFIX** 

    Checks if the given `TRIE` contains some prefix of the given string S.
    
    Returns the length of the matched prefix.

- [function] **TRIE-BUILD** 

    Builds a Trie based on the given list of patterns.

- [function] **INITIALIZE-AC** 

    Returns a Trie that is used to look for the given patterns in the
    text. It can deal either with a single pattern or a list of patterns.

- [function] **SEARCH-AC** 

    Looks for patterns that are indexed in the given trie and returns two values:
    start position of the first matching pattern and its index.

- [function] **STRING-CONTAINS-AC** 

    Looks for the given pattern in the text and returns index of the
    first occurence.

## 4 Regular expressions

Parsing and interpreting regular expressions

### 4.1 Portable RE by Massung

This code was modified to make it portable accross different
Common Lisp implementations. The biggest difference is the regular
expressions parser: the original library used "defparser" facility
that is specific to LispWorks and is not available elsewhere.

Another modification is to rename the "thread" structure with its
"make-thread" constructor into less controversial "re-thread" and
"make-re-thread".

Special thanks to Chun Tian (binghe) <binghe.lisp@gmail.com> for
his defparser-to-yacc function in the ASN.1 library.

It uses the simple non-recursive backtracking implementation from the:
[Regular Expression Matching: the Virtual Machine
Approach](https://swtch.com/~rsc/regexp/regexp2.html) paper by Russ
Cox.

#### 4.1.1 Compiling Patterns

To create a `RE` object, you can either use the `COMPILE-RE` function or
the `#/` dispatch macro.

        CL-USER > (compile-re "%d+")
        #<RE "%d+">
    
        CL-USER > #/%d+/
        #<RE "%d+">

Both work equally well, but the dispatch macro will compile the
pattern at read-time. The `RE` class has a [load
form](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ld_.htm#make-load-form)
and so can be saved to a FASL file.

*HINT: when using the read macro, use a backslash to escape the `/`
 and other characters that might mess with syntax coloring.*

Finally, the `WITH-RE` macro let's you user either strings or `RE`
objects in a body of code. If a string is passed as the pattern, then
it will be compiled before the body is evaluated.

        CL-USER > (with-re (re "%d+") re)
        #<RE "%d+">

*Note*: All pattern matching functions use the `WITH-RE` macro, and so
the pattern argument can be either a string or a pre-compiled `RE`
object.

- [class] **RE**

    Regular expression.

- [class] **RE-MATCH**

    Matched pattern.

- [function] **COMPILE-RE** *PATTERN*

    Create a regular expression from a pattern string.

- [macro] **WITH-RE** *(RE PATTERN) &BODY BODY*

    Compile pattern if it's not a `RE` object and execute body.

#### 4.1.2 Basic Pattern Matching

The heart of all pattern matching is the `MATCH-RE` function.

- [function] **MATCH-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) EXACT*

    Test a pattern re against a string.

It will match `string` against `pattern` and return a `RE-MATCH`
object on success or `nil` on failure. The `start` and `end` arguments
limit the scope of the match and default to the entire string. If
`exact` is `t` then the pattern has to consume the entire string (from
start to end).

        CL-USER > (match-re "%d+" "abc 123")
        NIL
    
        CL-USER > (match-re "%a+" "abc 123")
        #<RE-MATCH "abc">

Once you have successfully matched and have a `RE-MATCH` object, you
can use the following reader functions to inspect it:

- [reader] **MATCH-STRING** *RE-MATCH* *(:MATCH)*

- [reader] **MATCH-GROUPS** *RE-MATCH* *(:GROUPS)*

- [reader] **MATCH-POS-START** *RE-MATCH* *(:START-POS)*

- [reader] **MATCH-POS-END** *RE-MATCH* *(:END-POS)*

Try peeking into a match...

`CL-USER` > (inspect (match-re "(a(b(c)))" "abc 123"))
`MATCH`          "abc"
`GROUPS`         ("abc" "bc" "c")
`START-POS`      0
`END-POS`        3

#### 4.1.3 Pattern Scanning

To find a pattern match anywhere in a string use the `FIND-RE` function.

    (find-re pattern string &key start end all)

It will scan `string` looking for matches to `pattern`. If `all` is
non-`nil` then a list of all matches found is returned, otherwise it
will simply be the first match.

    CL-USER > (find-re "%d+" "abc 123")
    #<RE-MATCH "123">
    
    CL-USER > (find-re "[^%s]+" "abc 123" :all t)
    (#<RE-MATCH "abc">
     #<RE-MATCH "123">)


- [function] **FIND-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) ALL*

    Find a regexp pattern match somewhere in a string. Run from an offset.

#### 4.1.4 Splitting by Pattern

Once patterns have been matched, splitting a string from the
matches is trivial.

    (split-re pattern string &key start end all coalesce-seps)

If `all` is true, then a list of all sub-sequences in
`string` (delimited by `pattern`) are returned, otherwise just the
first and the rest of the string.

If `coalesce-seps` is true the sub-sequences that are empty will be
excluded from the results. This argument is ignored if `all` is `nil`.

    CL-USER > (split-re "," "1,2,3")
    "1"
    "2,3"
    
    CL-USER > (split-re "," "1,2,,,abc,3,," :all t :coalesce-seps t)
    ("1" "2" "abc" "3")


- [function] **SPLIT-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) ALL COALESCE-SEPS*

    Split a string into one or more strings by pattern match.

#### 4.1.5 Replacing by Pattern

The `REPLACE-RE` function scans the string looking for matching
sub-sequences that will be replaced with another string.

    (replace-re pattern with string &key start end all)

If `with` is a function, then the function is called with the `RE-MATCH`
object, replacing the pattern with the return value. Otherwise the
value is used as-is. As with `FIND-RE` and `SPLIT-RE`, if `all` is true,
then the pattern is globally replaced.

    CL-USER > (replace-re "%d+" #\* "1 2 3")
    "* 2 3"
    
    CL-USER > (replace-re "%a+" #'(lambda (m) (length (match-string m))) "a bc def" :all t)
    "1 2 3"

*NOTE: The string returned by `REPLACE-RE` is a completely new
 string. This is true even if `pattern` isn't found in the string.*

- [function] **REPLACE-RE** *PATTERN WITH S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) ALL*

    Replace patterns found within a string with a new value.

#### 4.1.6 Groups

Using parenthesis in a pattern will cause the matching text to be
groups in the returned `re-match` object. The `MATCH-GROUPS` function
will return a list of all the captured strings in the match.

    CL-USER > (match-groups (match-re #/(%d+)(%a+)/ "123abc"))
    ("123" "abc")

Captures can be nested, but are always returned in the order they are
**opened**.

    CL-USER > (match-groups (match-re #/(a(b(c)))(d)/ "abcd"))
    ("abc" "bc" "c" "d")

*HINT: you can always use the `MATCH-STRING` function to get at the full
 text that was matched and there's no need to capture the entire
 pattern.*

- [reader] **MATCH-GROUPS** *RE-MATCH* *(:GROUPS)*

#### 4.1.7 The \`with-re-match\` Macro

Whe `WITH-RE-MATCH` macro can be used to assist in extracting the
matched patterns and groups.

    (with-re-match ((var match-expr &key no-match) &body body)

If the result of `match-expr` is `nil`, then `no-match` is returned
and `body` is not executed.

While in the body of the macro, `$$` will be bound to the
`match-string` and the groups will be bound to `$1`, `$2`, ...,
`$9`. Any groups beyond the first 9 are bound in a list to `$_`.

    CL-USER > (with-re-match (m (match-re "(%a+)(%s+)(%d+)" "abc 123"))
                (string-append $3 $2 $1)))
    "123 abc"
    
    CL-USER > (flet ((initial (m)
                       (with-re-match (v m)
                         (format nil "~a." $1))))
                (replace-re #/(%a)%a+%s*/ #'initial "Lisp In Small Pieces" :all t))
    "L.I.S.P."


- [macro] **WITH-RE-MATCH** *(MATCH MATCH-EXPR &KEY NO-MATCH) &BODY BODY*

    Intern match symbols to execute a body.

  [03b3]: #x-28-22cl-string-match-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"cl-string-match\" ASDF/SYSTEM:SYSTEM)"
  [079c]: #x-28CL-STRING-MATCH-3A-40BOYER-MOORE-HORSPOOL-SECTION-20MGL-PAX-3ASECTION-29 "Boyer-Moore-Horspool algorithm"
  [182a]: #x-28CL-STRING-MATCH-3A-40KNUTH-MORRIS-PRATT-SECTION-20MGL-PAX-3ASECTION-29 "Knuth-Morris-Pratt algorithm"
  [19de]: #x-28CL-STRING-MATCH-3A-40PRE-BASIC-MATCHING-SECTION-20MGL-PAX-3ASECTION-29 "Basic Pattern Matching"
  [3d1e]: #x-28CL-STRING-MATCH-3A-40PRE-PATTERN-REPLACING-SECTION-20MGL-PAX-3ASECTION-29 "Replacing by Pattern"
  [43a4]: #x-28CL-STRING-MATCH-3A-40PRE-PATTERN-SPLITTING-SECTION-20MGL-PAX-3ASECTION-29 "Splitting by Pattern"
  [4e8e]: #x-28CL-STRING-MATCH-3A-40PRE-REGEXP-SECTION-20MGL-PAX-3ASECTION-29 "Portable RE by Massung"
  [4f5e]: #x-28CL-STRING-MATCH-3A-40AHO-CORASICK-SECTION-20MGL-PAX-3ASECTION-29 "Aho-Corasick algorithm"
  [5556]: #x-28CL-STRING-MATCH-3A-40RABIN-KARP-SECTION-20MGL-PAX-3ASECTION-29 "Rabin-Karp algorithm"
  [6eb5]: #x-28CL-STRING-MATCH-3A-40BRUTE-FORCE-SECTION-20MGL-PAX-3ASECTION-29 "Brute force"
  [74de]: #x-28CL-STRING-MATCH-3A-40PRE-PATTERN-SCANNING-SECTION-20MGL-PAX-3ASECTION-29 "Pattern Scanning"
  [7a26]: #x-28CL-STRING-MATCH-3A-40PRE-GROUPS-SECTION-20MGL-PAX-3ASECTION-29 "Groups"
  [82cb]: #x-28CL-STRING-MATCH-3A-40BOYER-MOORE-SECTION-20MGL-PAX-3ASECTION-29 "Boyer-Moore algorithm"
  [8daa]: #x-28CL-STRING-MATCH-3A-40SINGLE-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29 "Single pattern search"
  [c54a]: #x-28CL-STRING-MATCH-3A-40PRE-COMPILING-PATTERNS-SECTION-20MGL-PAX-3ASECTION-29 "Compiling Patterns"
  [df99]: #x-28CL-STRING-MATCH-3A-40REGEXP-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29 "Regular expressions"
  [dff8]: #x-28CL-STRING-MATCH-3A-40SHIFT-OR-SECTION-20MGL-PAX-3ASECTION-29 "Shift-OR algorithm"
  [f81a]: #x-28CL-STRING-MATCH-3A-40PRE-WITH-RE-MATCH-SECTION-20MGL-PAX-3ASECTION-29 "The \\`with-re-match\\` Macro"
  [ffe3]: #x-28CL-STRING-MATCH-3A-40MULTI-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29 "Multiple pattern search"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
