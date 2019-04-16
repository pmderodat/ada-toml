with Ada.Strings.Unbounded;
with Ada.Text_IO;

with TOML;
with TOML.File_IO;

procedure Main is
   package TIO renames Ada.Text_IO;

   Value : constant TOML.TOML_Value :=
      TOML.File_IO.Load_File ("example.toml").Value;
begin
   for E of Value.Iterate_On_Table loop
      TIO.Put_Line (Ada.Strings.Unbounded.To_String (E.Key) & " is a "
                    & E.Value.Kind'Image);
   end loop;
end Main;
