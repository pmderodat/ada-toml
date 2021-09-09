with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with TOML;

procedure Main is
   Table1 : constant TOML.TOML_Value := TOML.Create_Table;
   Table2 : constant TOML.TOML_Value := TOML.Create_Table;
   Table3 : constant TOML.TOML_Value := TOML.Create_Table;
   Arr    : constant TOML.TOML_Value := TOML.Create_Array;
begin
   --  Populate tables
   Table1.Set ("a", TOML.Create_Integer (1));

   Arr.Append (TOML.Create_Integer (1));
   Table2.Set ("b", Arr);

   Table3.Set ("a", TOML.Create_Integer (3));

   --  With the resolver-less Merge overload, expect a constraint error on
   --  duplicate keys.
   declare
      Dummy : TOML.TOML_Value;
   begin
      Dummy := TOML.Merge (Table1, Table3);
      Ada.Text_IO.Put_Line ("No exception...");
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line
           ("Merging two tables with duplicate keys raises an exception");
   end;
   Ada.Text_IO.New_Line;

   --  With the resolver overload, check that conflicts are resolved as
   --  expected
   Ada.Text_IO.Put_Line ("Merging with conflict resolution...");
   declare
      function Merge_Entries
        (Key  : TOML.Unbounded_UTF8_String;
         L, R : TOML.TOML_Value) return TOML.TOML_Value
      is (TOML.Create_String (Key & ": " & L.As_String & " | " & R.As_String));

      L : constant TOML.TOML_Value := TOML.Create_Table;
      R : constant TOML.TOML_Value := TOML.Create_Table;

      Merged : TOML.TOML_Value;
   begin
      L.Set ("a", TOML.Create_String ("good"));
      L.Set ("b", TOML.Create_String ("hello"));

      R.Set ("b", TOML.Create_String ("world"));
      R.Set ("c", TOML.Create_String ("bye"));

      Merged := TOML.Merge (L, R, Merge_Entries'Access);

      Ada.Text_IO.Put_Line (Merged.Dump_As_String);
   end;

   declare
      Merged : constant TOML.TOML_Value := TOML.Merge (Table1, Table2);
   begin
      -- Change array value to see the shallow copy modified
      Arr.Append (TOML.Create_Integer (2));

      Ada.Text_IO.Put_Line ("Merged table:");
      Ada.Text_IO.Put_Line (Merged.Dump_As_String);
   end;
end Main;
