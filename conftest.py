import hy
  # For the side-effect of allowing import of Hy programs.

import pytest

def pytest_collect_file(parent, path):
    if path.basename.startswith('test_') and path.ext == ".hy":
        return pytest.Module.from_parent(parent, fspath=path)
