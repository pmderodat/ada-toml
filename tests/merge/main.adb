with Ada.Text_IO;

with TOML;

procedure Main is
   Table1 : constant TOML.TOML_Value := TOML.Create_Table;
   Table2 : constant TOML.TOML_Value := TOML.Create_Table;
   Arr    : constant TOML.TOML_Value := TOML.Create_Array;
begin
   --  Populate tables
   Table1.Set ("a", TOML.Create_Integer (1));
 
   Arr.Append (TOML.Create_Integer (1));
   Table2.Set ("b", Arr);
   

   declare
      Merged : constant TOML.TOML_Value := TOML.Merge (Table1, Table2);
   begin
      -- Change array value to see the shallow copy modified
      Arr.Append (TOML.Create_Integer (2));

      Ada.Text_IO.Put_Line (Merged.Dump_As_String);
   end;
end Main;
