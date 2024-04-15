Hyrule is a utility library for the `Hy <http://hylang.org>`_ programming language. It can be thought of as the Hy equivalent, or addition, to Python's standard library. While intended primarily for Hy programs, its functions and classes can be used in Python as with any other Python library; just ``import hyrule``. Hyrule's macros, on the other hand, are only really usable in Hy.

All of Hyrule's contents can be imported or required directly from the top-level module ``hyrule``, as in ``(require hyrule [branch])``. The specific submodule an object belongs to may not be stable between releases.

`Hyrule's documentation can be read online on Hylang.org. <http://hylang.org/hyrule/doc>`_

You can run Hyrule's test suite with the command ``pytest`` and build its documentation with ``( cd docs; sphinx-build . _build -b html )``.
