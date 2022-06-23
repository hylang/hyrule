.. default-role:: code

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
