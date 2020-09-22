Running ada-toml's testsuite
============================

Running the testsuite requires:

* A Python interpreter and the `e3-testsuite
  <https://github.com/AdaCore/e3-testsuite>`_ library (for the testsuite
  framework). See the Python setup section below for a quick guide.

Make sure to build the library and the checkers first:

.. code-block:: sh

   $ gprbuild -Pada_toml -p
   $ gprbuild -Pcheckers -p

And then execute the ``run-tests.py`` script:

.. code-block:: sh

   $ ./run-tests.py

This script accept several arguments (see ``--help``). The most useful ones
are:

* ``-j`` (for instance ``-j8``) to run tests in parallel;
* ``-E`` to show logs in case of test failures.

When the testsuite finishes, various logs can be found in the ``out/new``
directory.


Python setup
------------

The first step is to install a Python2 interpreter and ``virtualenv``. For
instance, on Debian-based GNU/Linux distributions, you just have to install the
``python-virtualenv`` package. You can then run the following commands:

.. code-block:: sh

   # Create a virtualenv (prefix to install packages). The exact command varies
   # from one Linux distribution to another: virtualenv, virtualenv2,
   # virtualenv-2.7, ...
   $ virtualenv2 my-virtual-env

   # Update your environment to use it
   $ source my-virtual-env/bin/activate

   # Install e3-testsuite and all its dependencies
   $ pip install e3-testsuite

   # You should now be able to run the testsuite:
   $ ./run-tests.py
