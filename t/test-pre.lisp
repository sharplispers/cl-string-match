;;; This is a test suit for PRE that is based on: regexp-test-suite.cl
;;; developed by Sébastien SAINT-SEVIN and is a part of the
;;; CLAWK/regex library.
;;;
;;; The original sources are currently being hosted on GitHub:
;;;
;;; https://github.com/michaelw/regex/blob/master/regexp-test-suite.lisp
;;;
;;; Some of the tests are removed as well as some comments and
;;; commented-out code.
;;;
;;; Some tests are modified to follow the syntax of the Portable RE
;;; engine.
;;;
;;; sbcl --load 'test-pre.lisp' --eval '(run-portable-re-tests)' --quit
;;;
;;; Current Portable RE version still fails several tests, but the
;;; rest passes fine. Also we don't check for group matches.
;;;
;;; See comments marked with "todo" for corresponding comments.

;; -----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-string-match))

(defparameter *regexp-tests*
  '(
    ;; *********************************************************************
    ;; <test> ::= (<string>  <pattern>  <compile-p>  <matched-p>  <results>)
    ;; <results> ::= (<global match> <group_capture>*)
    ;; ---------------------------------------------------------------------


    ;; *******************************************************
    ;; the tests that follows are from:
    ;; -------------------------------------------------------
    ;; (c) Sudhir Shenoy, 1996
    ;;
    ;; The tests here are from:
    ;;
    ;; (a) Tom Lord's GNU rx package
    ;; (b) from the Zebu parser generator package
    ;;     (modified to use new syntax)
    ;; -------------------------------------------------------
    ;; All have been slightly modified to follow the syntax
    ;; Used by CLAWK/regex module - Sébastien Saint-Sevin, 2002
    ;; -------------------------------------------------------

    ("a*a*"           "aaaaaa"        t t       ("aaaaaa"))
    ("a*a*a*"         "aaaaaa"        t t       ("aaaaaa"))
    ("a*a*a*a*"       "aaaaaa"        t t       ("aaaaaa"))
    ("a*a*a*a*a*"     "aaaaaa"        t t       ("aaaaaa"))
    ("a*a*a*a*a*a*"   "aaaaaa"        t t       ("aaaaaa"))
    ("a*a*a*a*a*a*a*" "aaaaaa"        t t       ("aaaaaa"))

    (""               ""              nil nil   ())
    ("b{0,6}"         ""              t t       (""))
    ("ab{0,0}c"       "abc"           t nil     ())
    ("ab{1,1}c"       "abbc"          t nil     ())
    ("ab{3,7}c"       "abbbbbbbbc"    t nil     ())
    ("ab{3,7}c"       "abbbbbbbbbc"   t nil     ())
    ("ab{3,7}c"       "abbbbbbbbbbc"  t nil     ())
    ("ab{3,7}c"       "abbbbbbbbbbbc" t nil     ())
    ("b{2,7}"         "bb"            t t       ("bb"))
    ("b{1,6}"         ""              t nil     ())
    ("b{1,6}"         "b"             t t       ("b"))
    ("b{2,7}"         "b"             t nil     ())
    ("ab{0,7}c"       "ac"            t t       ("ac"))
    ("ab{1,7}c"       "abc"           t t       ("abc"))
    ("ab{2,7}c"       "abbc"          t t       ("abbc"))
    ("ab{3,7}c"       "abbbc"         t t       ("abbbc"))
    ("ab{3,7}c"       "abbbbc"        t t       ("abbbbc"))
    ("ab{3,7}c"       "abbbbbc"       t t       ("abbbbbc"))
    ("ab{3,7}c"       "abbbbbbc"      t t       ("abbbbbbc"))
    ("ab{3,7}c"       "abbbbbbbc"     t t       ("abbbbbbbc"))
    ("ab{3,7}c"       "abbbbbbbbc"    t nil     ())
    ("ab{3,7}c"       "abbc"          t nil     ())
    ("ab{3,7}c"       "abc"           t nil     ())

    ("(a|b)*c|(a|ab)*c" "xc"          t t       ("c" "" ""))
    ("(a)*"           "b"             t t       ("" ""))
    ("(..)*(...)*"    "a"             t t       ("" "" ""))
    ("(..)*(...)*"    "abc"           t t       ("ab" "ab" ""))

    ("^"              ""              t t       (""))
    ("$"              ""              t t       (""))
    ("^$"             ""              t t       (""))
    ("^a$"            "a"             t t       ("a"))
    ("abc"            "abc"           t t       ("abc"))
    ("abc"            "xbc"           t nil     ())
    ("abc"            "axc"           t nil     ())
    ("abc"            "abx"           t nil     ())
    ("abc"            "xabcy"         t t       ("abc"))
    ("abc"            "ababc"         t t       ("abc"))
    ("ab*c"           "abc"           t t       ("abc"))
    ("ab*bc"          "abc"           t t       ("abc"))
    ("ab*bc"          "abbc"          t t       ("abbc"))
    ("ab*bc"          "abbbbc"        t t       ("abbbbc"))
    ("ab+bc"          "abbc"          t t       ("abbc"))
    ("ab+bc"          "abc"           t nil     ())
    ("ab+bc"          "abq"           t nil     ())
    ("ab+bc"          "abbbbc"        t t       ("abbbbc"))
    ("ab?bc"          "abbc"          t t       ("abbc"))
    ("ab?bc"          "abc"           t t       ("abc"))
    ("ab?bc"          "abbbbc"        t nil     ())
    ("ab?c"           "abc"           t t       ("abc"))
    ("^abc$"          "abc"           t t       ("abc"))
    ("^abc$"          "abcc"          t nil     ())
    ("^abc"           "abcc"          t t       ("abc"))
    ("^abc$"          "aabc"          t nil     ())
    ("abc$"           "aabc"          t t       ("abc"))
    ("^"              "abc"           t t       (""))
    ("$"              "abc"           t t       (""))
    ("a.c"            "abc"           t t       ("abc"))
    ("a.c"            "axc"           t t       ("axc"))
    ("a.*c"           "axyzc"         t t       ("axyzc"))
    ("a.*c"           "axyzd"         t nil     ())

    ("a[bc]d"         "abc"           t nil     ())
    ("a[bc]d"         "abd"           t t       ("abd"))
    ("a[b-d]e"        "abd"           t nil     ())
    ("a[b-d]e"        "ace"           t t       ("ace"))
    ("a[b-d]"         "aac"           t t       ("ac"))
    ("a[-b]"          "a-"            t t       ("a-"))
    ("a[b-]"          "a-"            t t       ("a-"))

    ("a[^bc]d"        "aed"           t t       ("aed"))
    ("a[^bc]d"        "abd"           t nil     ())
    ("a[^-b]c"        "adc"           t t       ("adc"))
    ("a[^-b]c"        "a-c"           t nil     ())
    ;; todo
    ;; ("a[^\]b]c"      "a]c"           t nil     ())
    ;; ("a[^\]b]c"      "adc"           t t       ("adc"))
    ("ab|cd"          "abc"           t t       ("ab"))
    ("ab|cd"          "abcd"          t t       ("ab"))

    ;; todo: this pattern crashes Lisp machine
    ;; ("^*"             "-"             t t       (""))
    ("$*"             "-"             t t       (""))
    ("$b"             "b"             t nil     ())

    ;; todo: deal with groups
    ;; ("a\\(b"          "a(b"           t t       ("a(b"))
    ;; ("a\\(*b"         "ab"            t t       ("ab"))
    ;; ("a\\(*b"         "a((b"          t t       ("a((b"))
    ;; ("a\\\\b"         "a\\b"          t t       ("a\\b"))
    ;; ("(abc"           "-"             nil nil   ())
    ;; ("((a))"          "abc"           t t       ("a" "a" "a"))
    ;; ("(a)b(c)"        "abc"           t t       ("abc" "a" "c"))
    ("a+b+c"          "aabbabc"       t t       ("abc"))

    ;; ("a**"            "-"             t t       (""))
    ;; ("a*?"            "-"             t t       (""))

    ;; todo: the following patterns crash Lisp machine
    ;; ("(a*)*"          "-"             t t       ("" ""))
    ;; ("(a*)+"          "-"             t t       ("" ""))
    ;; ("(a|)*"          "-"             t t       ("" ""))
    ;; ("(a*|b)*"        "-"             t t       ("" ""))
    ("(a+|b)?"        "ab"            t t       ("a"  "a"))
    ("[^ab]*"         "cde"           t t       ("cde"))
    ("(^)*"           "-"             t t       ("" ""))
    ;; todo: the following pattern crashes Lisp machine
    ;; ("(ab|)*"         "-"             t t       ("" ""))
    (")("             "-"             nil nil   ())
    (""               "abc"           nil nil   ())
    ("abc"            ""              t nil     ())
    ("a*"             ""              t t       (""))
    ("([abc])*bcd"    "abcd"          t t       ("abcd"   "a"))
    ("a|b|c|d|e"      "e"             t t       ("e"))
    ("(a|b|c|d|e)f"   "ef"            t t       ("ef" "e"))
    ;; todo: the following pattern crashes Lisp machine
    ;; ("((a*|b))*"      "-"             t t       ("" "" ""))
    ("abcd*efg"       "abcdefg"       t t       ("abcdefg"))
    ("ab*"            "xabyabbbz"     t t       ("ab"))
    ("ab*"            "xayabbbz"      t t       ("a"))
    ("(ab|cd)e"       "abcde"         t t       ("cde" "cd"))
    ("[abhgefdc]ij"   "hij"           t t       ("hij"))
    ("^(ab|cd)e"      "abcde"         t nil     ())
    ("(abc|)ef"       "abcdef"        t t       ("ef" ""))
    ("(a|b)c*d"       "abcd"          t t       ("bcd" "b"))
    ("(ab|ab*)bc"     "abc"           t t       ("abc" "a"))
    ("a([bc]*)c*"     "abc"           t t       ("abc"  "bc"))
    ("a([bc]*)(c*d)"  "abcd"          t t       ("abcd" "bc" "d"))
    ("a([bc]+)(c*d)"  "abcd"          t t       ("abcd" "bc" "d"))
    ("a([bc]*)(c+d)"  "abcd"          t t       ("abcd" "b" "cd"))
    ("a[bcd]*dcdcde"  "adcdcde"       t t       ("adcdcde"))
    ("a[bcd]+dcdcde"  "adcdcde"       t nil     ())
    ("(ab|a)b*c"      "abc"           t t       ("abc"  "ab"))
    ("((a)(b)c)(d)"   "abcd"          t t       ("abcd" "abc" "a" "b" "d"))
    ("[a-zA-Z_][a-zA-Z0-9_]*" "alpha" t t       ("alpha"))
    ("^a(bc+|b[eh])g|.h$"     "abh"   t t       ("bh" ""))
    ("(bc+d$|ef*g.|h?i(j|k))" "effgz" t t       ("effgz" "effgz" ""))
    ("(bc+d$|ef*g.|h?i(j|k))" "ij"    t t       ("ij" "ij" "j"))
    ("(bc+d$|ef*g.|h?i(j|k))" "effg"  t nil     ())
    ("(bc+d$|ef*g.|h?i(j|k))" "bcdd"  t nil     ())
    ("(bc+d$|ef*g.|h?i(j|k))" "reffgz" t t      ("effgz" "effgz" ""))


    ("((((((((((a))))))))))"  "a"       t t     ("a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a"))
    ("(((((((((a)))))))))"    "a"       t t     ("a" "a" "a" "a" "a" "a" "a" "a" "a" "a"))
    ("multiple words of text" "uh-uh"   t nil   ())
    ("multiple words"         "multiple words, yeah" t t ("multiple words"))
    ("(.*)c(.*)"              "abcde"   t t     ("abcde" "ab" "de"))
    ("\\((.*), (.*)\\)"       "(a, b)"  t t     ("(a, b)" "a" "b"))
    ("[k]"                    "ab"      t nil   ())
    ("abcd"                   "abcd"    t t     ("abcd"))
    ("a(bc)d"                 "abcd"    t t     ("abcd" "bc"))
    ("a[-]?c"                 "ac"      t t     ("ac"))
    ("a[-]?c"               "ac"     t t     ("ac"))
    ("a[-]?c"               "ac"      t t     ("ac"))
    ("[ -~]*"                 "abc"     t t     ("abc"))
    ("[ -~ -~]*"              "abc"     t t     ("abc"))
    ("[ -~ -~ -~]*"           "abc"     t t     ("abc"))
    ("[ -~ -~ -~ -~]*"        "abc"     t t     ("abc"))
    ("[ -~ -~ -~ -~ -~]*"     "abc"     t t     ("abc"))
    ("[ -~ -~ -~ -~ -~ -~]*"  "abc"     t t     ("abc"))
    ("[ -~ -~ -~ -~ -~ -~ -~]*" "abc"   t t     ("abc"))
    ;;
    ;; Tests from from the Zebu package (originally for nregex.lisp)
    ;;
    ("(na)x+"                 "naxna"      t t     ("nax" "na"))
    ("(na)x+na"               "naxna123"   t t     ("naxna" "na"))
    ("(na)x+"                 "naxxos"     t t     ("naxx" "na"))
    ("(na)x+"                 "naxos"      t t     ("nax" "na"))
    ("(na)x+"                 "naos"       t nil   ())
    ("(na)x*"                 "naxxos"     t t     ("naxx" "na"))
    ("(na)x*"                 "naxos"      t t     ("nax" "na"))
    ("(na)x*"                 "naos"       t t     ("na" "na"))
    ("[0-9]+"                 "123ab"      t t     ("123"))
    ("[a-zA-Z]+"              "aAbb123"    t t     ("aAbb"))
    ("[0-9a-z]+"              "1234&&*"    t t     ("1234"))
    ("[0-9a-z]+"              "1234a&&*"   t t     ("1234a"))
    ("[0-9a-zA-Z]+"           "a1234a"     t t     ("a1234a"))
    ("[0-9a-zA-Z&]+"          "aAbb123&&*" t t     ("aAbb123&&"))
    ("[0-9]+\\.[0-9]*"        "0.123cm"    t t     ("0.123"))

    ("ca?r"          "car"          t t   ("car"))
    ("ca?r"          "cr"           t t   ("cr"))
    ("c[ad]+r"       "caaar"        t t   ("caaar"))
    ("c[ad]+r"       "caaar aa1"    t t   ("caaar"))
    ("c[ad]+r$"      "caaar"        t t   ("caaar"))
    (".*"            ""             t t   (""))
    (".*"            "aa"           t t   ("aa"))
    ("c[ad]?r"       "cr"           t t   ("cr"))
    ("c[ad]?r"       "car"          t t   ("car"))
    ("c[ad]?r"       "cdr"          t t   ("cdr"))
    ("c[0-9]?r"      "cr"           t t   ("cr"))
    ("c[0-9]?r"      "c9rxx"        t t   ("c9r"))
    ("c[0-9]?r"      "crxx"         t t   ("cr"))
    ("a|b"           "a"            t t   ("a"))
    ("ab.yz"         "ab yz"        t t   ("ab yz"))
    ("(abc){1,2}"    "abcabc"       t t   ("abcabc" "abc"))
    ("a|bc*"         "a"            t t   ("a"))
    ("[A-Z]+"        "ABCY"         t t   ("ABCY"))
    ("[0-9]+\\.[0-9]*(e[+-]?[0-9]+)"    "12.3e4  k"    t t   ("12.3e4" "e4"))
    ("[0-9]+\\.[0-9]*(e[+-]?[0-9]+)"    "12.3e-4  k"   t t   ("12.3e-4" "e-4"))
    ("[0-9]+\\.[0-9]*(e[+-]?[0-9]+)?"   "12.3  k"      t t   ("12.3" ""))
    ;;
    ;; The Gadaffi tests
    ;; Note that the first group matches NULL because it is always sucked
    ;; up by the preceding .* in case of a successful match.
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Qaddafi"       t t ("Muammar Qaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mo'ammar Gadhafi"      t t ("Mo'ammar Gadhafi" "" "dh"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Kaddafi"       t t ("Muammar Kaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Qadhafi"       t t ("Muammar Qadhafi" "" "dh"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Moammar El Kadhafi"    t t ("Moammar El Kadhafi" "" "dh"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Gadafi"        t t ("Muammar Gadafi" "" "d"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mu'ammar al-Qadafi"    t t ("Mu'ammar al-Qadafi" "" "d"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Moamer El Kazzafi"     t t ("Moamer El Kazzafi" "" "zz"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Moamar al-Gaddafi"     t t ("Moamar al-Gaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mu'ammar Al Qathafi"   t t ("Mu'ammar Al Qathafi" "" "th"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Al Qathafi"    t t ("Muammar Al Qathafi" "" "th"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mo'ammar el-Gadhafi"   t t ("Mo'ammar el-Gadhafi" "" "dh"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Moamar El Kadhafi"     t t ("Moamar El Kadhafi" "" "dh"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar al-Qadhafi"    t t ("Muammar al-Qadhafi" "" "dh"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mu'ammar al-Qadhdhafi" t t ("Mu'ammar al-Qadhdhafi" "" "dh"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mu'ammar Qadafi"       t t ("Mu'ammar Qadafi" "" "d"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Moamar Gaddafi"        t t ("Moamar Gaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mu'ammar Qadhdhafi"    t t ("Mu'ammar Qadhdhafi" "" "dh"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Khaddafi"      t t ("Muammar Khaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar al-Khaddafi"   t t ("Muammar al-Khaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mu'amar al-Kadafi"     t t ("Mu'amar al-Kadafi" "" "d"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Ghaddafy"      t t ("Muammar Ghaddafy" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Ghadafi"       t t ("Muammar Ghadafi" "" "d"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Ghaddafi"      t t ("Muammar Ghaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muamar Kaddafi"        t t ("Muamar Kaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Quathafi"      t t ("Muammar Quathafi" "" "th"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muammar Gheddafi"      t t ("Muammar Gheddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Muamar Al-Kaddafi"     t t ("Muamar Al-Kaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Moammar Khadafy "      t t ("Moammar Khadafy" "" "d"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Moammar Qudhafi"       t t ("Moammar Qudhafi" "" "dh"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mu'ammar al-Qaddafi"   t t ("Mu'ammar al-Qaddafi" "" "dd"))
    ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
     "Mulazim Awwal Mu'ammar Muhammad Abu Minyar al-Qadhafi" t t
     ("Mu'ammar Muhammad Abu Minyar al-Qadhafi" "" "dh"))


    ;; *******************************************************
    ;; the tests that follows are from:
    ;; -------------------------------------------------------
    ;; Sébastien Saint-Sevin, 2002
    ;; -------------------------------------------------------

    ;; some basics
    ;; -----------
    (".*"             "aa"        t t ("aa"))
    (".+"             "aa"        t t ("aa"))


    ;; anchor
    ;; ------


    ;; alternate
    ;; ---------
    ("(hello|man|)"   ""          t t ("" ""))
    ("(a+|b)"         "aaa"       t t ("aaa" "aaa"))
    ("(a+|b)"         "b"         t t ("b" "b"))


    ;; character classes
    ;; -----------------
    ("[abc]{1,3}"       "bcaa"    t t ("bca"))


    ("a[\\-]?c"         "ac"      t t ("ac"))
    ("a[\\-]?c"         "a-c"     t t ("a-c"))
    ("a[-]?c"           "ac"      t t ("ac"))
    ("a[-]?c"           "a-c"     t t ("a-c"))
    ("a[-b]?c"          "abc"     t t ("abc"))
    ("a[b-]?c"          "acc"     t t ("ac"))


    ;; posix character classes
    ;; -----------------------


    ;; greedy quantifiers
    ;; ------------------
    ("a*"           "aaaa"        t t        ("aaaa"))
    ("a+"           "aaaa"        t t        ("aaaa"))
    ("a{2,3}"       "aaaa"        t t        ("aaa"))


    ;; nongreedy quantifiers
    ;; ---------------------
    ;; todo: the following patterns are not parsed/interpreted properly
    ;; ("a*?"          "aaaa"        t t        (""))
    ;; ("a+?"          "aaaa"        t t        ("a"))
    ("a{2,3}?"      "aaaa"        t t        ("aa"))


    ;; todo: the following patterns are not parsed/interpreted properly
    ;; ("a+?bb*?"      "baaaabaaabbbaaaaa"     t t    ("aaaab"))
    ;; ("a+?bb+?"      "baaaabaaabbbaaaaa"     t t    ("aaabb"))

    ("[abc]{10,20}?" "xxxbcbcbabcaabcbabcbcbabcbcaabcabxxx"  t t ("bcbcbabcaa"))

    ("(a+)*"                  "aaaa"    t t        ("aaaa"    "aaaa"))
    ("(a+)*"                  "aaaa"    t t        ("aaaa"    "aaaa"))

    ))


(defun run-portable-re-tests ()

  (print ";; *****************************************************************************")
  (print ";; BEGIN OF TEST")
  (print ";; -----------------------------------------------------------------------------")
  (dolist (test *regexp-tests*)
    (destructuring-bind (pattern str expected-compile-p expected-matched-p expected-results) test
      (format t "~%pattern: ~A ~%string: ~A" pattern str)
      (when expected-compile-p
	(let ((matcher (sm:compile-re pattern)))
	  (cond ((and matcher (not expected-compile-p))
		 (format t "~%Shouldn't have compiled, but did ******************** TEST FAILED"))
		((and (not matcher) expected-compile-p)
		 (format t "~%Should have compiled, but didn't ******************** TEST FAILED"))
		)
	  (when matcher
	    (let ((match (sm:find-re matcher str)))
	      (cond ((and expected-matched-p (not match))
		     (format t "~%Should have matched, but didn't ******************** TEST FAILED"))
		    ((and (not expected-matched-p) match)
		     (format t "~%Shouldn't have matched, but did ******************** TEST FAILED"))
		    )
	      (when match
		(if (string= (car expected-results) (subseq str
							    (sm:match-pos-start match)
							    (sm:match-pos-end match)))
		    (format t "~%Global match OK" )
		    (format t "~%Global match ******************** TEST FAILED")
		    )
		;; todo: implement group match tests
		#+ignore
		(let ((num-groups (array-dimension (sm:match-groups match) 0))
		      )
		  (when (/= (length expected-results) num-groups)
		    (format t "~%Number of groups ******************** TEST FAILED")
		    )))
	      ))))
      )
    (terpri))
  (print ";; *****************************************************************************")
  (print ";; END OF TEST")
  (print ";; -----------------------------------------------------------------------------")
  )


;; *****************************************************************************
;; END OF FILE
;; -----------------------------------------------------------------------------
