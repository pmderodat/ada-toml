with Ada.Text_IO;

with TOML;
with TOML.File_IO;

procedure Main is
   Result : constant TOML.Read_Result :=
      TOML.File_IO.Load_File ("nosuchfile.toml");
begin
   Ada.Text_IO.Put_Line (TOML.Format_Error (Result));
end Main;
