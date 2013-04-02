CL-ORG-MODE-PARSER - Parsing org-mode files.

Copyright (C) 2013 Olof-Joachim Frahm

Released under a Simplified BSD license.

Implementations currently running on: SBCL.  Should be portable though.

Uses fiveam for testing and a couple of general purpose libraries.

There are actually a lot of other packages for the same purpose.  The
aim of this one is to parse org-mode files via a SAX-like event
interface into e.g. CLOS-based documents and to offer some options with
regards to parsed elements, interning of tags and similar things.

The parser is currently line- and regex-based, however it might be
feasible to use a parser generator instead.  Since there probably is no
single definite grammar (?) and as many documents as possible should be
parsed (with warnings though), I'll leave it at that for the moment.


# REPLSHOT

Provided you have a handler, parsing a document is in the simplest case
only a call to `PARSE`, which by default uses `DOCUMENT-BUILDER`:

    > (cl-org-mode-parser:parse #P"foo.org")
    > =>
    > #<CL-ORG-MODE-PARSER:DOCUMENT with 2 nodes {1003867DA3}>


# TESTING

Although there are already some test files, it would generally be nice
to compare a lot of libraries and their handling of various edge cases.
This includes e.g. missing spaces between elements, which however could
still be parsed by relaxing the rules a bit (and providing an option to
toggle this behaviour).


# TODO

- figure out whether the `CHARACTERS` handler should remain line-based,
  or be like in SAX with arbitrary chunks of text as input
- how much trouble is the whole "keep the printed representation"
  really (worth)?
- basic roundtrip
- include line/column information in errors
- proper subclasses for (continuable) errors
- parse a lot more special syntax (tables, drawers, ...)
- integrate the fiveam tests into the ASDF definition
