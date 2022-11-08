#!/usr/bin/env python

import setuptools
from setuptools.command.install import install

with open('README.rst', 'r') as o:
    long_description = o.read()


class install(install):
    def run(self):
        super().run()
        import py_compile

        import hy  # for compile hooks

        for path in set(self.get_outputs()):
            if path.endswith(".hy"):
                py_compile.compile(
                    path,
                    invalidation_mode=py_compile.PycInvalidationMode.CHECKED_HASH,
                )


# both setup_requires and install_requires
# since we need to compile .hy files during setup
requires = [
    'hy >= 0.24.0, < 0.26'
]


setuptools.setup(
    name = 'hyrule',
    version = '0.2.1',
    setup_requires=['wheel'] + requires,
    install_requires=requires,
    packages = setuptools.find_packages(),
    package_data={'': ['*.hy']},
    author = "Paul Tagliamonte",
    author_email = "tag@pault.ag",
    description = 'A utility library for the Hy programming language',
    long_description = long_description,
    license = 'Expat',
    project_urls = dict(
        Documentation = 'https://hyrule.readthedocs.io',
        Source = 'https://github.com/hylang/hyrule'),
    platforms = ['any'],
    classifiers = [
        "Development Status :: 4 - Beta",
        "License :: DFSG approved",
        "License :: OSI Approved :: MIT License",  # Really "Expat". Ugh.
        "Operating System :: OS Independent",
        "Programming Language :: Lisp",
        "Programming Language :: Python",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10"],
    cmdclass={'install': install})
