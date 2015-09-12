This directory contains code that is not directly invloved with the
pattern search algorithms but nevertheless might be found useful for
text handling/processing. Currently it contains:

* `ascii-strings.lisp` aims to provide single-byte strings
  functionality for Unicode-enabled Common Lisp
  implementations. Another goal is to reduce memory footprint and
  boost performance of the string-processing tasks, i.e. `read-line`.

* `scanf.lisp` a trivial scanf-like functionality implementation
