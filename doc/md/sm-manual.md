<a id='x-28CL-STRING-MATCH-3A-40CL-STRING-MATCH-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# CL-STRING-MATCH Manual

## Table of Contents

- [1 cl-string-match ASDF System Details][03b3]
- [2 Single pattern search][8daa]
- [3 Multiple pattern search][ffe3]
- [4 Regular expressions][df99]
    - [4.1 Portable RE by Massung][4e8e]

###### \[in package CL-STRING-MATCH\]
[`CL-STRING-MATCH`][03b3] String and pattern matching library reference.

<a id='x-28-22cl-string-match-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 cl-string-match ASDF System Details

- Version: 2017.12.25
- Description: Provides implementations of the standard sub-string search (string
matching) algorithms: brute-force, Boyer-Moore, Rabin-Karp, etc.
- Licence: BSD
- Author: Vityok https://bitbucket.org/vityok
- Homepage: [https://bitbucket.org/vityok/cl-string-match](https://bitbucket.org/vityok/cl-string-match)

<a id='x-28CL-STRING-MATCH-3A-40SINGLE-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29'></a>

## 2 Single pattern search

Looking for a single pattern in a string

<a id='x-28CL-STRING-MATCH-3ASTRING-CONTAINS-BRUTE-20FUNCTION-29'></a>

- [function] **STRING-CONTAINS-BRUTE** 

    A Brute-force substring search implementation.
    
    Brute-force substring search requires O(N x M) character compares to
    search for a pattern of length M in a text of length N, in the worst
    case.
    
    Algorithm described in: Chapter 5, p. 760 in
      'Algorithms', Robert Sedgewick and Kevin Wayne. 4th

<a id='x-28CL-STRING-MATCH-3A-40MULTI-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29'></a>

## 3 Multiple pattern search

Looking for multiple patterns in a string

<a id='x-28CL-STRING-MATCH-3ASEARCH-AC-20FUNCTION-29'></a>

- [function] **SEARCH-AC** 

    Looks for patterns that are indexed in the given trie and returns two values:
    start position of the first matching pattern and its index.

<a id='x-28CL-STRING-MATCH-3A-40REGEXP-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29'></a>

## 4 Regular expressions

Parsing and interpreting regular expressions

<a id='x-28CL-STRING-MATCH-3A-40PRE-REGEXP-SECTION-20MGL-PAX-3ASECTION-29'></a>

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

<a id='x-28CL-STRING-MATCH-3ARE-20CLASS-29'></a>

- [class] **RE**

    Regular expression.

<a id='x-28CL-STRING-MATCH-3ARE-MATCH-20CLASS-29'></a>

- [class] **RE-MATCH**

    Matched pattern.

<a id='x-28CL-STRING-MATCH-3AWITH-RE-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **WITH-RE** *(RE PATTERN) &BODY BODY*

    Compile pattern if it's not a `RE` object and execute body.

<a id='x-28CL-STRING-MATCH-3AWITH-RE-MATCH-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **WITH-RE-MATCH** *(MATCH MATCH-EXPR &KEY NO-MATCH) &BODY BODY*

    Intern match symbols to execute a body.

<a id='x-28CL-STRING-MATCH-3ACOMPILE-RE-20FUNCTION-29'></a>

- [function] **COMPILE-RE** *PATTERN*

    Create a regular expression from a pattern string.

<a id='x-28CL-STRING-MATCH-3AMATCH-RE-20FUNCTION-29'></a>

- [function] **MATCH-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) EXACT*

    Test a pattern re against a string.

<a id='x-28CL-STRING-MATCH-3AFIND-RE-20FUNCTION-29'></a>

- [function] **FIND-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) ALL*

    Find a regexp pattern match somewhere in a string. Run from an offset.

<a id='x-28CL-STRING-MATCH-3ASPLIT-RE-20FUNCTION-29'></a>

- [function] **SPLIT-RE** *PATTERN S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) ALL COALESCE-SEPS*

    Split a string into one or more strings by pattern match.

<a id='x-28CL-STRING-MATCH-3AREPLACE-RE-20FUNCTION-29'></a>

- [function] **REPLACE-RE** *PATTERN WITH S &KEY (START 0) (END (LENGTH S)) (OFFSET 0) ALL*

    Replace patterns found within a string with a new value.

<a id='x-28CL-STRING-MATCH-3AMATCH-STRING-20GENERIC-FUNCTION-29'></a>

- [generic-function] **MATCH-STRING** *OBJECT*

<a id='x-28CL-STRING-MATCH-3AMATCH-GROUPS-20GENERIC-FUNCTION-29'></a>

- [generic-function] **MATCH-GROUPS** *OBJECT*

<a id='x-28CL-STRING-MATCH-3AMATCH-POS-START-20GENERIC-FUNCTION-29'></a>

- [generic-function] **MATCH-POS-START** *OBJECT*

<a id='x-28CL-STRING-MATCH-3AMATCH-POS-END-20GENERIC-FUNCTION-29'></a>

- [generic-function] **MATCH-POS-END** *OBJECT*

  [03b3]: #x-28-22cl-string-match-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"cl-string-match\" ASDF/SYSTEM:SYSTEM)"
  [4e8e]: #x-28CL-STRING-MATCH-3A-40PRE-REGEXP-SECTION-20MGL-PAX-3ASECTION-29 "Portable RE by Massung"
  [8daa]: #x-28CL-STRING-MATCH-3A-40SINGLE-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29 "Single pattern search"
  [df99]: #x-28CL-STRING-MATCH-3A-40REGEXP-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29 "Regular expressions"
  [ffe3]: #x-28CL-STRING-MATCH-3A-40MULTI-PATTERN-SEARCH-20MGL-PAX-3ASECTION-29 "Multiple pattern search"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
