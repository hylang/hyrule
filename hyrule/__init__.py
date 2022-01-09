# We use an `__init__.py` instead of `__init__.hy` so that importing
# from Python works even if `hy` hasn't been imported yet.

__version__ = '0.1'

import hy
hy.macros.require('hyrule.hy_init',
   # The Python equivalent of `(require hyrule.hy-init *)`
   None, assignments = 'ALL', prefix = '')
from hyrule.hy_init import *
