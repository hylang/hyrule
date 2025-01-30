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

.. hy:autotag:: %
.. hy:automacro:: ap-if
.. hy:automacro:: ap-each
.. hy:automacro:: ap-each-while
.. hy:automacro:: ap-map
.. hy:automacro:: ap-map-when
.. hy:automacro:: ap-filter
.. hy:automacro:: ap-reject
.. hy:automacro:: ap-dotimes
.. hy:automacro:: ap-first
.. hy:automacro:: ap-last
.. hy:automacro:: ap-reduce
.. hy:automacro:: ap-when
.. hy:automacro:: ap-with

``argmove`` — Macros for calls with unusual argument placement 
----------------------------------------------------------------------
.. hy:automodule:: hyrule.argmove

.. hy:automacro:: ->
.. hy:automacro:: ->>
.. hy:automacro:: as->
.. hy:automacro:: some->
.. hy:automacro:: doto

``collections`` — Tools for data structures
----------------------------------------------------------------------
.. hy:automodule:: hyrule.collections

.. hy:autofunction:: assoc
.. hy:automacro:: ncut
.. hy:autotag:: s

``control`` — Control structures
----------------------------------------------------------------------
.. hy:automodule:: hyrule.control

.. hy:automacro:: block
.. hy:automacro:: branch
.. hy:automacro:: case
.. hy:automacro:: cfor
.. hy:automacro:: defmain
.. hy:automacro:: do-n
.. hy:automacro:: ebranch
.. hy:automacro:: ecase
.. hy:automacro:: lif
.. hy:automacro:: list-n
.. hy:automacro:: loop
.. hy:autoclass:: recur
.. hy:automacro:: unless

``destructure`` — Macros for destructuring collections
----------------------------------------------------------------------
.. hy:automodule:: hyrule.destructure

API
~~~~~~~~~~

.. hy:automacro:: defn+
.. hy:automacro:: dict=:
.. hy:automacro:: fn+
.. hy:automacro:: let+
.. hy:automacro:: setv+

``iterables`` — Tools for iterable objects
----------------------------------------------------------------------
.. hy:automodule:: hyrule.iterables

.. hy:autofunction:: butlast
.. hy:autofunction:: coll?
.. hy:autofunction:: distinct
.. hy:autofunction:: drop-last
.. hy:autofunction:: flatten
.. hy:autofunction:: rest
.. hy:autofunction:: thru

``macrotools`` — Tools for writing and handling macros
----------------------------------------------------------------------
.. hy:automodule:: hyrule.macrotools

.. hy:autotag:: /
.. hy:automacro:: def-gensyms
.. hy:automacro:: defmacro-kwargs
.. hy:automacro:: defmacro!
.. hy:autofunction:: macroexpand-all
.. hy:autofunction:: map-hyseq
.. hy:autofunction:: map-model
.. hy:autofunction:: match-fn-params

``oop`` — Tools for object-oriented programming
----------------------------------------------------------------------
.. hy:automodule:: hyrule.oop

.. hy:automacro:: meth
.. hy:automacro:: ameth

``pprint`` — Pretty-printing data structures
----------------------------------------------------------------------
.. hy:automodule:: hyrule.hypprint

.. hy:autoclass:: PrettyPrinter
.. hy:autofunction:: pformat
.. hy:autofunction:: pp
.. hy:autofunction:: pprint
.. hy:autofunction:: readable?
.. hy:autofunction:: recursive?
.. hy:autofunction:: saferepr

``sequences`` — Cached, indexable iterables
----------------------------------------------------------------------
.. hy:automodule:: hyrule.sequences

.. hy:autoclass:: Sequence
.. hy:automacro:: seq
.. hy:automacro:: defseq
.. hy:autofunction:: end-sequence

``misc`` — Everything else
----------------------------------------------------------------------
.. hy:automodule:: hyrule.misc

.. hy:automacro:: comment
.. hy:autofunction:: constantly
.. hy:autofunction:: dec
.. hy:autofunction:: inc
.. hy:autofunction:: import-path
.. hy:automacro:: of
.. hy:autofunction:: parse-args
.. hy:automacro:: profile/cpu
.. hy:automacro:: pun
.. hy:autofunction:: sign
.. hy:automacro:: smacrolet
.. hy:autofunction:: xor

Contributing to Hyrule
======================

.. include:: ../CONTRIBUTING.rst
