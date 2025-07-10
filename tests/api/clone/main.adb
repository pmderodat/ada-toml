with Ada.Text_IO;

with TOML;
with TOML.File_IO;

procedure Main is
   Value : constant TOML.TOML_Value :=
      TOML.File_IO.Load_File ("example.toml").Value;
   Clone : constant TOML.TOML_Value := Value.Clone;
begin
   --  Mutate Value: the Clone is supposed to be unaffected

   Value.Set ("hello", TOML.Create_Boolean (true));
   Value.Get ("array").Append (TOML.Create_Boolean (false));

   --  Verify table in array is a new copy (fix in #45)
   Value.Get ("array_of_table")
        .Item (1)
        .Set ("extra", TOML.Create_String ("new_value"));

   Ada.Text_IO.Put_Line (Clone.Dump_As_String);
end Main;
