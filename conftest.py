import hy
  # For the side-effect of allowing import of Hy programs.
import pytest

def pytest_collect_file(file_path, parent):
    if file_path.name.startswith('test_') and file_path.suffix == '.hy':
        return pytest.Module.from_parent(parent, path = file_path)
