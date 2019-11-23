Contributing to ada-toml
========================

Everyone is welcome to contribute to this project, provided that the following
rules are respected.


Where/how?
----------

Please report bugs and start any discussions about the project on `GitHub
issues <https://github.com/pmderodat/ada-toml/issues>`_. If you report bug,
make sure to provide enough information so that the bug can be investigated:
for instance, provide the TOML file that triggers a parsing bug, or a
compileable and executable set of sources for a memory corruption bug.

If you want to submit patches, please open a pull request.  `This blog post
<https://eli.thegreenplace.net/2019/how-to-send-good-pull-requests-on-github/>`_.
provides nice guidelines for such contributions.


Coding style
------------

This project follows `GNAT's coding style
<https://gcc.gnu.org/onlinedocs/gnat-style/index.html>`_. Please use it when
sending patches.


Documentation
-------------

Given how basic the API for ada-toml is right now, there is no standalone
user's guide. Instead, care is taken to make the public API (``toml.ads``,
``toml-file_io.ads``, etc.) as much self-documented as possible, and a short
tutorial is present in the `project README <README.rst>`_.

Note that non trivial parts of the implementation, such as the parser, are
required to be well self-documented.


Licensing
---------

``ada-toml`` is distributed under the `3-Clause BSD License <LICENSE>`_. When
sending patches to ada-toml, you agree that your contributions will be licensed
under that license.
