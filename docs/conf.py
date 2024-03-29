import sys, os; sys.path.insert(0, os.path.abspath('..'))
  # Read the Docs needs this bit to import Hyrule.

html_title = 'The Hyrule manual'

extensions = [
    'sphinx.ext.napoleon',
    'sphinx.ext.autodoc',
    'sphinx.ext.intersphinx',
    'sphinxcontrib.hydomain']

exclude_patterns = ['_build']

smartquotes = False
nitpicky = True

highlight_language = "hylang"

intersphinx_mapping = dict(
    py = ('https://docs.python.org/3/', None),
    hy = ('https://docs.hylang.org/en/master', None))
