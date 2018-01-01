# CL-STRING-MATCH Manual

## Table of Contents

- 1 cl-string-match ASDF System Details
- 2 Single pattern search
- 3 Multiple pattern search
- 4 Regular expressions
    - 4.1 Portable RE by Massung
        - 4.1.1 Compiling Patterns
        - 4.1.2 Basic Pattern Matching

###### \[in package CL-STRING-MATCH\]
`CL-STRING-MATCH` String and pattern matching library reference.

## 1 cl-string-match ASDF System Details

- Version: 2017.12.25
- Description: Provides implementations of the standard sub-string search (string
matching) algorithms: brute-force, Boyer-Moore, Rabin-Karp, etc.
- Licence: BSD
- Author: Vityok https://bitbucket.org/vityok
- Homepage: [https://bitbucket.org/vityok/cl-string-match](https://bitbucket.org/vityok/cl-string-match)

## 2 Single pattern search

Looking for a single pattern in a string

- [function] **STRING-CONTAINS-BRUTE** 

    A Brute-force substring search implementation.
    
    Brute-force substring search requires O(N x M) character compares to
    search for a pattern of length M in a text of length N, in the worst
    case.
    
    Algorithm described in: Chapter 5, p. 760 in
      'Algorithms', Robert Sedgewick and Kevin Wayne. 4th

## 3 Multiple pattern search

Looking for multiple patterns in a string

- [function] **SEARCH-AC** 

    Looks for patterns that are indexed in the given trie and returns two values:
    start position of the first matching pattern and its index.

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

- [class] **RE**

    Regular expression.

- [class] **RE-MATCH**

    Matched pattern.

#### 4.1.1 Compiling Patterns

To create a `RE` object, you can either use the `COMPILE-RE` function or the `#/` dispatch macro.

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

- [function] **COMPILE-RE** *PATTERN*

    Create a regular expression from a pattern string.

- [macro] **WITH-RE** *(RE PATTERN) &BODY BODY*

    Compile pattern if it's not a `RE` object and execute body.

#### 4.1.2 Basic Pattern Matching

The heart of all pattern matching is the `match-re` function.

- [function] **MATCH-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) EXACT*

    Test a pattern re against a string.

It will match `string` against `pattern` and return a `re-match`
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

- [macro] **WITH-RE-MATCH** *(MATCH MATCH-EXPR &KEY NO-MATCH) &BODY BODY*

    Intern match symbols to execute a body.

- [function] **MATCH-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) EXACT*

    Test a pattern re against a string.

- [function] **FIND-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) ALL*

    Find a regexp pattern match somewhere in a string. Run from an offset.

- [function] **SPLIT-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) ALL COALESCE-SEPS*

    Split a string into one or more strings by pattern match.

- [function] **REPLACE-RE** *PATTERN WITH S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) ALL*

    Replace patterns found within a string with a new value.

  [03b3]: #x-28-22cl-string-match-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"cl-string-match\" ASDF/SYSTEM:SYSTEM)"
  [19de]: #x-28CL-STRING-MATCH-3A-40PRE-BASIC-MATCHING-SECTION-20MGL-PAX-3ASECTION-29 "Basic Pattern Matching"
  [4e8e]: #x-28CL-STRING-MATCH-3A-40PRE-REGEXP-SECTION-20MGL-PAX-3ASECTION-29 "Portable RE by Massung"
  [8daa]: #x-28CL-STRING-MATCH-3A-40SINGLE-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29 "Single pattern search"
  [c54a]: #x-28CL-STRING-MATCH-3A-40PRE-COMPILING-PATTERNS-SECTION-20MGL-PAX-3ASECTION-29 "Compiling Patterns"
  [df99]: #x-28CL-STRING-MATCH-3A-40REGEXP-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29 "Regular expressions"
  [ffe3]: #x-28CL-STRING-MATCH-3A-40MULTI-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29 "Multiple pattern search"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
