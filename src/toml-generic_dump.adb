with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure TOML.Generic_Dump (Stream : in out Output_Stream; Value : TOML_Value)
is

   procedure Put (Bytes : String);
   --  Shortcut for Put (Stream, Bytes)

   type Map_Pair is record
      Key   : Unbounded_UTF8_String;
      Value : TOML_Value;
   end record;

   type Map_Pair_Array is array (Positive range <>) of Map_Pair;

   function Append_Key
     (Prefix, Suffix : Unbounded_UTF8_String) return Unbounded_UTF8_String;
   --  Return a key corresponding to "Prefix.Suffix" (if Prefix is not empty)
   --  or "Suffix" (if Prefix is empty).

   procedure Separate_Pairs
     (Pairs           : in out Map_Pair_Array;
      Last_Table_Pair : out Natural;
      Last_Array_Pair : out Natural)
      with Pre => Pairs'First = 1;
   --  Shuffle Pairs to that, after the procedure returns:
   --
   --  * Pairs (Pairs'First .. Last_Table_Pair) contains only table values;
   --  * Pairs (Last_Table_Pair + 1 .. Last_Array_Pair) contains only
   --    array-of-tables values;
   --  * Pairs (Last_Array_Pair + 1 .. Pairs'Last) contains remaining values.
   --
   --  The three array sub-ranges are sorted by key.

   procedure Put_Table_Header
     (Prefix, Suffix : Unbounded_UTF8_String;
      Nested_Key     : out Unbounded_UTF8_String);
   --  Set Nested_Key to Append_Key (Prefix, Suffix). Then write a table header
   --  for Nested_Key to Stream.

   procedure Put_Array_Header (Nested_Key : Unbounded_UTF8_String);
   --  Write an array of tables header for Nested_Key to Stream

   procedure Dump_Toplevel_Table
     (Key : Unbounded_UTF8_String; Value : TOML_Value)
      with Pre => Value.Kind = TOML_Table;
   --  Dump the given table (Value) as a top-level table, under the given key

   procedure Dump_Toplevel_Array
     (Parent_Key, Array_Key : Unbounded_UTF8_String; Array_Value : TOML_Value)
      with Pre => Array_Value.Kind = TOML_Array;
   --  Dump the given array (Array_Value) as a top-level array of tables, under
   --  the given keys (Parent_Key "." Array_Key).

   procedure Dump_Inline (Value : TOML_Value)
      with Pre => Value /= No_TOML_Value;
   --  Dump the given value using the inline format

   ---------
   -- Put --
   ---------

   procedure Put (Bytes : String) is
   begin
      Put (Stream, Bytes);
   end Put;

   ----------------
   -- Append_Key --
   ----------------

   function Append_Key
     (Prefix, Suffix : Unbounded_UTF8_String) return Unbounded_UTF8_String is
   begin
      --  TODO: quote keys when needed

      if Length (Prefix) = 0 then
         return Suffix;
      else
         return Prefix & "." & Suffix;
      end if;
   end Append_Key;

   --------------------
   -- Separate_Pairs --
   --------------------

   procedure Separate_Pairs
     (Pairs           : in out Map_Pair_Array;
      Last_Table_Pair : out Natural;
      Last_Array_Pair : out Natural)
   is
      type Pair_List is record
         Pairs : Map_Pair_Array (Separate_Pairs.Pairs'Range);
         Last  : Natural := 0;
      end record;

      procedure Append (Pair : Map_Pair; List : in out Pair_List);
      --  Append Pair to the given List

      procedure Append
        (List : Pair_List; First_Pair : Positive; Last_Pair : out Natural);
      --  Append all pairs in List to Pairs. The first one goes to:
      --
      --    Pairs (First_Pair)
      --
      --  and the last one is copied to:
      --
      --    Pairs (Last_Pair)

      ------------
      -- Append --
      ------------

      procedure Append (Pair : Map_Pair; List : in out Pair_List) is
      begin
         List.Last := List.Last + 1;
         List.Pairs (List.Last) := Pair;
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append
        (List : Pair_List; First_Pair : Positive; Last_Pair : out Natural)
      is
      begin
         Last_Pair := First_Pair - 1;
         for P of List.Pairs (1 .. List.Last) loop
            Last_Pair := Last_Pair + 1;
            Pairs (Last_Pair) := P;
         end loop;
      end Append;

      Table_Pairs, Array_Pairs, Other_Pairs : Pair_List;
      Last                                  : Natural;
   begin
      --  Put pairs in separate lists. Note that this preserves sorting.

      for P of Pairs loop
         case P.Value.Kind is
            when TOML_Table =>
               Append (P, Table_Pairs);
            when TOML_Array =>
               if P.Value.Item_Kind = TOML_Table then
                  Append (P, Array_Pairs);
               else
                  Append (P, Other_Pairs);
               end if;
            when others =>
               Append (P, Other_Pairs);
         end case;
      end loop;

      --  Then put back each category of pairs to Pairs

      Append (Table_Pairs, Pairs'First, Last_Table_Pair);
      Append (Array_Pairs, Last_Table_Pair + 1, Last_Array_Pair);
      Append (Other_Pairs, Last_Array_Pair + 1, Last);
      pragma Assert (Last = Pairs'Last);
   end Separate_Pairs;

   ----------------------
   -- Put_Table_Header --
   ----------------------

   procedure Put_Table_Header
     (Prefix, Suffix : Unbounded_UTF8_String;
      Nested_Key     : out Unbounded_UTF8_String) is
   begin
      Nested_Key := Append_Key (Prefix, Suffix);
      Put ("[" & To_String (Nested_Key) & "]" & ASCII.LF);
   end Put_Table_Header;

   ----------------------
   -- Put_Array_Header --
   ----------------------

   procedure Put_Array_Header (Nested_Key : Unbounded_UTF8_String) is
   begin
      Put ("[[" & To_String (Nested_Key) & "]]" & ASCII.LF);
   end Put_Array_Header;

   -------------------------
   -- Dump_Toplevel_Table --
   -------------------------

   procedure Dump_Toplevel_Table
     (Key : Unbounded_UTF8_String; Value : TOML_Value)
   is
      Keys                             : constant Key_Array := Value.Keys;
      Pairs                            : Map_Pair_Array (Keys'Range);
      Last_Table_Pair, Last_Array_Pair : Natural;
      Nested_Key                       : Unbounded_UTF8_String;
   begin
      --  Initialize Pairs from Keys and Value and split pairs by value kind

      for I in Keys'Range loop
         Pairs (I) := (Key   => Keys (I),
                       Value => Value.Get (Keys (I)));
      end loop;
      Separate_Pairs (Pairs, Last_Table_Pair, Last_Array_Pair);

      declare
         Table_Pairs : Map_Pair_Array renames
            Pairs (Pairs'First ..  Last_Table_Pair);
         Array_Pairs : Map_Pair_Array renames
            Pairs (Last_Table_Pair + 1 .. Last_Array_Pair);
         Other_Pairs : Map_Pair_Array renames
            Pairs (Last_Array_Pair + 1 .. Pairs'Last);
      begin
         --  Dump non-table and non-array map pairs with inline style:
         --  key = value

         for Pair of Other_Pairs loop
            Put (To_String (Pair.Key));
            Put (" = ");
            Dump_Inline (Pair.Value);
            Put ((1 => ASCII.LF));
         end loop;

         --  Dump tables as top-level ones: [section]

         for Pair of Table_Pairs loop
            Put_Table_Header (Key, Pair.Key, Nested_Key);
            Dump_Toplevel_Table (Nested_Key, Pair.Value);
         end loop;

         --  Dump arrays as top-level ones: [[section]]

         for Pair of Array_Pairs loop
            Dump_Toplevel_Array (Key, Pair.Key, Pair.Value);
         end loop;
      end;
   end Dump_Toplevel_Table;

   -------------------------
   -- Dump_Toplevel_Array --
   -------------------------

   procedure Dump_Toplevel_Array
     (Parent_Key, Array_Key : Unbounded_UTF8_String; Array_Value : TOML_Value)
   is
      Nested_Key : constant Unbounded_UTF8_String :=
         Append_Key (Parent_Key, Array_Key);
   begin
      for I in 1 .. Array_Value.Length loop
         Put_Array_Header (Nested_Key);
         Dump_Toplevel_Table (Nested_Key, Array_Value.Item (I));
      end loop;
   end Dump_Toplevel_Array;

   -----------------
   -- Dump_Inline --
   -----------------

   procedure Dump_Inline (Value : TOML_Value) is
   begin
      case Value.Kind is
         when TOML_Boolean =>
            Put ((if Value.As_Boolean then "true" else "false"));

         when TOML_Integer =>
            declare
               --  Make sure to strip the leading space, if present

               Image : constant String := Any_Integer'Image (Value.As_Integer);
               First : constant Positive :=
                 (if Image (Image'First) = ' '
                  then Image'First + 1
                  else Image'First);
            begin
               Put (Image (First .. Image'Last));
            end;

         when TOML_String =>
            --  TODO: escape strings when needed

            Put ("""" & Value.As_String & """");

         when TOML_Array =>
            Put ("[" & ASCII.LF);
            for I in 1 .. Value.Length loop
               Dump_Inline (Value.Item (I));
               Put ("," & ASCII.LF);
            end loop;
            Put ("]");

         when others =>
            --  TODO: implement dump for other kinds of values
            raise Program_Error;
      end case;
   end Dump_Inline;

begin
   Dump_Toplevel_Table (Null_Unbounded_String, Value);
end TOML.Generic_Dump;
