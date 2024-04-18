import time

extensions = [
    'sphinx.ext.napoleon',
    'sphinx.ext.intersphinx',
    'sphinx.ext.autodoc',
    'sphinxcontrib.hydomain']

import warnings; import sphinx.deprecation as SD
for c in (SD.RemovedInSphinx60Warning, SD.RemovedInSphinx70Warning):
    warnings.filterwarnings('ignore', category = c)

import hyrule

project = 'Hy'
copyright = '%s the authors' % time.strftime('%Y')
html_title = f'Hyrule {hyrule.__version__} manual'

hy_version = 'v0.28.0'

exclude_patterns = ['_build']

html_theme = 'nature'
html_theme_options = dict(
    nosidebar = True,
    body_min_width = 0,
    body_max_width = 'none')
html_css_files = [f'https://hylang.org/hy/doc/{hy_version}/_static/custom.css']
html_copy_source = False
html_show_sphinx = False

nitpicky = True
add_module_names = False
smartquotes = False

highlight_language = 'hylang'

intersphinx_mapping = dict(
    py = ('https://docs.python.org/3/', None),
    hy = (f'https://hylang.org/hy/doc/{hy_version}/', None))
