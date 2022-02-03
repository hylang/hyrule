.. module:: hyrule

====================================
The Hyrule manual
====================================

.. include:: ../README.rst

.. contents::
   :local:

Reference
=========

``anaphoric`` — Anaphoric macros
----------------------------------------------------------------------

.. hy:automodule:: hyrule.anaphoric
  :macros: #%, ap-if, ap-each, ap-each-while, ap-map, ap-map-when,
    ap-filter, ap-reject, ap-dotimes, ap-first, ap-last, ap-reduce

``argmove`` — Macros for calls with unusual argument placement 
----------------------------------------------------------------------

.. hy:automodule:: hyrule.argmove
  :macros: ->, ->>, as->, doto

``collections`` — Tools for data structures
----------------------------------------------------------------------

.. hy:automodule:: hyrule.collections
  :members: postwalk, prewalk, walk
  :macros: #:, assoc, ncut

``control`` — Control structures
----------------------------------------------------------------------

.. hy:automodule:: hyrule.control
  :macros: block, branch, ebranch, case, ecase, cfor, defmain, do-n,
    ifp, lif, list-n, loop, unless

``destructure`` — Macros for destructuring collections
----------------------------------------------------------------------

.. hy:automodule:: hyrule.destructure
  :macros: defn+, defn/a+, dict=:, fn+, fn/a+, let+, setv+

``iterables`` — Tools for iterable objects
----------------------------------------------------------------------

.. hy:automodule:: hyrule.iterables
  :members: butlast, coll?, distinct, drop-last, flatten, rest

``macrotools`` — Tools for writing and handling macros
----------------------------------------------------------------------

.. hy:automodule:: hyrule.macrotools
  :members: macroexpand-all
  :macros: defmacro/g!, defmacro!, with-gensyms

``pprint`` — Pretty-printing data structures
----------------------------------------------------------------------

.. hy:automodule:: hyrule.hypprint
  :members: PrettyPrinter, pformat, pp, pprint, saferepr,
      readable?, recursive?

``sequences`` — Lazy, indexable iterables
----------------------------------------------------------------------

.. hy:automodule:: hyrule.sequences
  :members: end-sequence
  :macros: defseq, seq

``misc`` — Everything else
----------------------------------------------------------------------

.. hy:automodule:: hyrule.misc
  :members: constantly, dec, inc, parse-args, xor
  :macros: comment, of, profile/calls, profile/cpu, smacrolet

Contributing to Hyrule
======================

.. include:: ../CONTRIBUTING.rst
