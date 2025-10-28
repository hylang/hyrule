.. default-role:: code

Hyrule is `semantically versioned <https://semver.org/>`__ since 1.0.0.

Unreleased
======================================================

New Features
------------------------------
* New function ``sqlite-db``.

Misc. Improvements
------------------------------
* Fixed a setuptools build issue by moving build dependencies to a new `pyproject.toml`

1.0.0 (released 2025-03-19)
======================================================

Removals
------------------------------
* `defn/a+` has been removed. Use `(defn+ :async …)` instead.
* `fn/a+` has been removed. Use `(fn+ :async …)` instead.
* `profile/calls` has been removed. Use `pycallgraph2` or
  `python-call-graph`, which are available on PyPI, instead.
* `profile/cpu` has been removed. Use `(with [pr (cProfile.Profile)]
  …)` instead.

Other Breaking Changes
------------------------------
* `(with-gensyms [a b c] …)` is now written `(def-gensyms a b c) …`.
* The names of gensyms produced by `import-path` have changed. (This
  shouldn't break your code unless you're doing something
  exceptionally weird, but the argument to `hy.gensym` was documented,
  so this is technically a breaking change.)

Bug Fixes
------------------------------
* A lot of incompatibilities of `meth`, `ameth`, `defn+`, and `fn+`
  with the core `defn` and `fn` have been fixed. You can now use `:async`,
  docstrings are recognized properly, etc.
* `defmacro!` no longer crashes when mutating macro arguments.
* `match-fn-params` now handles `None` correctly.

0.8.0 (released 2025-01-08)
======================================================

Removals
------------------------------
* `walk`, `prewalk`, and `postwalk` have been removed. Use `map-model` and
  `map-hyseq` instead.

Breaking Changes
------------------------------
* `recur` is now a real object that must be imported from Hyrule when
  using `loop`.
* `macroexpand-all` now uses the same parameters as `hy.macroexpand`.
* `lif` now treats `False` as false.

New Features
------------------------------
* New macro `pun`.
* New macro `map-hyseq`.
* `loop` allows more kinds of parameters.
* `assoc` allows keyword arguments.
* `flatten`, given a non-collection, returns it as a singleton list,
  instead of raising an error.

Bug Fixes
------------------------------
* `map-model` now calls `as-model` only once (before its own recursion),
  and it does so unconditionally.
* `loop` now works when nested.
* `macroexpand-all` no longer crashes on stub macros.
* `macroexpand-all` now recognizes macro names properly.
* `lif` now works when renamed, or when invoked with `hy.R`.

0.7.0 (released 2024-09-22; uses Hy ≥ 1)
======================================================

New Features
------------------------------
* New macros `meth` and `ameth`.

Bug Fixes
------------------------------
* `match-fn-params` now raises an error for syntactically invalid
  parameter lists.

0.6.0 (released 2024-05-20; uses Hy 0.29.*)
======================================================

Removals
------------------------------
* `defmacro/g!` has been removed. Use `defmacro!` instead.

Other Breaking Changes
------------------------------
* `cut` on a `Sequence` now returns another `Sequence`, not a generator.
* `assoc` is now a function rather than a macro. Its effect is unchanged.

New Features
------------------------------
* The `Sequence` constructor now accepts an arbitrary iterable.
* New macro `defmacro-kwargs`.
* New macro `parse-fn-params`.
* New macro `some->`.
* New function `import-path`.
* New function `sign`.
* New function `thru`.

Bug Fixes
------------------------------
* `defmacro!` now properly handles explicit return statements
  within its macro definition.
* Various `Sequence` bugs have been fixed.
* `assoc` no longer requires a second and third argument.

0.5.0 (released 2024-01-05; uses Hy 0.28.*)
======================================================

No user-visible changes beyond compatibility with new Hy versions.

0.4.0 (released 2023-07-06; uses Hy 0.27.*)
======================================================

New Features
------------------------------
* `do-n` and `list-n` now allow the count to be `Inf`.
* New reader macro `/` that wraps `hy.M` for easy imports in macros.

Bug Fixes
------------------------------
* `seq` and `defseq` no longer crash if `Sequence` is not imported.
* `->`, `->>`, and `doto` now properly handle bare symbols with leading dot.

0.3.0 (released 2023-02-08; uses Hy 0.26.*)
======================================================

New Features
------------------------------
* New macros `ap-when`, `ap-with`.

0.2.1 (released 2022-11-08; uses Hy 0.24.0 or 0.25.*)
======================================================

Changed `setup.py` to declare compatibility with the new Hy release.

0.2 (released 2022-06-23; uses Hy 0.24.0)
==================================================

Removals
------------------------------
* `ifp` has been removed. Use `branch` instead.

Other Breaking Changes
------------------------------
* The reader macro (formerly tag macro) `:` is now named `s`.

New Features
------------------------------
* New macro `block`.
* New macros `branch`, `ebranch`, `case`, and `ecase`.
* The macros of `hyrule.anaphoric` are now much smarter in deciding
  what instances of the symbol `it` to replace, so you can now quote
  or locally rebind `it` without issues.

Bug Fixes
------------------------------
* Destructuring macros now consistently return `None` in case of
  failing to match a certain pattern, instead of sometimes returning
  `None` and sometimes raising an exception.
* `#%` now parses `%i` names from dotted symbols.
* The Hy code is now precompiled during installation.

0.1 (released 2022-01-09; uses Hy 1.0a4)
==================================================

This is the first release of Hyrule per se. The one change below is
described relative to the ancestors of Hyrule in Hy 1.0a3.

Breaking Changes
------------------------------
* `coll?` now returns `False` for `bytes` objects.
