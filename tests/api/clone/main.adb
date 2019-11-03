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

   Ada.Text_IO.Put_Line (Clone.Dump_As_String);
end Main;
