ada-toml: TOML parser for Ada
=============================

``ada-toml`` is a pure Ada library for parsing and creating `TOML
<https://github.com/toml-lang/toml#toml>`_ documents. It conforms to the
`version 1.0.0 <https://toml.io/en/v1.0.0>`_ of the format standard.


Build and install
-----------------

With an Ada 2012 compiler and GPRbuild, building and installing the library is
as simple as running:

.. code-block:: sh

   $ make
   $ sudo make install

If you don't want to install in ``/usr``, you can replace the last command
with:

.. code-block:: sh

   $ sudo make install prefix=/where/to/install

Note that the ``Makefile`` is just a wrapper around ``gprbuild`` and
``gprinstall`` to make it easy for users and packagers. You can also run them
manually:

.. code-block:: sh

   gprbuild -Pada_toml.gpr -p

This will build in debug mode and produce a static library. In order to build
in production mode, add ``-XBUILD_MODE=prod``, and to build a dynamic library,
add ``-XLIBRARY_TYPE=static``.

Installation to ``$PREFIX`` is simply done using GPRinstall:

.. code-block:: sh

   gprinstall -Pada_toml.gpr --prefix=$PREFIX

See `TESTING.rst
<https://github.com/pmderodat/ada_toml/tree/master/TESTING.rst>`_ to run the
testsuite.


Quick tutorial
--------------

All basic types and subprograms are in the ``TOML`` package. All "nodes" in a
TOML documents are materialized using the  ``TOML.TOML_Value`` type. Since TOML
values make up a tree, this type has reference semantics. This means that
modifying a TOML node does not modify the corresponding ``TOML_Value`` value
itself, but rather the TOML value that is referenced.

Parsing a TOML file is as easy as using the ``TOML.File_IO.Load_File`` function:

.. code-block:: ada

   declare
      Result : constant TOML.Read_Result :=
         TOML.File_IO.Load_File ("config.toml");
   begin
      if Result.Success then
         Ada.Text_IO.Put_Line ("config.toml loaded with success!");
      else
         Ada.Text_IO.Put_Line ("error while loading config.toml:");
         Ada.Text_IO.Put_Line
            (Ada.Strings.Unbounded.To_String (Result.Message));
      end if;
   end;

Each TOML value has kind, defining which data it contains (a boolean, an
integer, a string, a table, ...). To each kind, one or several primitives are
associated to let one process the underlying data:

.. code-block:: ada

   case Result.Kind is
      when TOML.TOML_Boolean =>
         Ada.Text_IO.Put_Line ("Boolean: " & Result.As_Boolean'Image);

      when TOML.TOML_Integer =>
         Ada.Text_IO.Put_Line ("Boolean: " & Result.As_Integer'Image);

      when TOML.TOML_String =>
         Ada.Text_IO.Put_Line ("Boolean: " & Result.As_String);

      when TOML.TOML_Array =>
         Ada.Text_IO.Put_Line ("Array of " & Result.Length & " elements");

      when others =>
         null;
   end case;

There are also primitives to build TOML values:

.. code-block:: ada

   declare
      Bool : constant TOML.TOML_Value := TOML.Create_Boolean (False);
      Int  : constant TOML.TOML_Value := TOML.Create_Integer (10);
      Str  : constant TOML.TOML_Value := TOML.Create_String ("Hello, world");

      Table : constant TOML.TOML_Value := TOML.Create_Table;
   begin
      Table.Set ("bool_field", Bool);
      Table.Set ("int_field", Int);
      Table.Set ("str_field", Str);
   end;

And finally one can turn a tree of TOML nodes back in text form:

.. code-block:: ada

   Ada.Text_IO.Put_Line ("TOML document:");
   Ada.Text_IO.Put_Line (Table.Dump_As_String);


Contributing
------------

The development of ``ada-toml`` happens on `GitHub
<https://github.com/pmderodat/ada_toml>`_.  Everyone is welcome to contribute
to this project: please read our `contribution rules
<https://github.com/pmderodat/ada_toml/tree/master/CONTRIBUTING.rst>`_ if you
consider doing so.
