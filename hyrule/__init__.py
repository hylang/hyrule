# We use an `__init__.py` instead of `__init__.hy` so that importing
# from Python works even if `hy` hasn't been imported yet.

__version__ = '1.0.0'

import hy
from hyrule.hy_init import *
hy.eval(hy.read('(require hyrule.hy-init :macros * :readers *)'))
