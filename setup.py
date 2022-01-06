#!/usr/bin/env python

import setuptools

with open('README.rst', 'r') as o:
    long_description = o.read()

setuptools.setup(
    name = 'hyrule',
    version = None,
    install_requires = [
        'hy @ git+https://github.com/hylang/hy@master#egg=hy-1.0'],
    extras_require = dict(
        docs = [
            'Sphinx == 3.5.4',
            'sphinxcontrib-hydomain @ git+https://github.com/hylang/sphinxcontrib-hydomain@master#egg=sphinxcontrib-hydomain-1.0']),
    packages = setuptools.find_packages(),
    package_data = {
        'hyrule': ['*.hy', '__pycache__/*']},
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
        "Programming Language :: Python :: 3.10"])
