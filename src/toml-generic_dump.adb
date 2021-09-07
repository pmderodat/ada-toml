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

   function Format_String
     (S : Unbounded_UTF8_String) return Unbounded_UTF8_String;
   --  Format a valid TOML representation of the given string (S)

   function Format_Key
     (Key : Unbounded_UTF8_String) return Unbounded_UTF8_String;
   --  Format a valid TOML representation of the given Key

   function Append_Key
     (Prefix, Suffix : Unbounded_UTF8_String) return Unbounded_UTF8_String;
   --  Return a key corresponding to "Prefix.Suffix" (if Prefix is not empty)
   --  or "Suffix" (if Prefix is empty).

   procedure Separate_Pairs
     (Pairs           : in out Map_Pair_Array;
      Last_Table_Pair : out Natural;
      Last_Array_Pair : out Natural)
      with Pre => Pairs'First = 1;
   --  Shuffle Pairs so that, after the procedure returns:
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

   function Strip_Number (Image : String) return String;
   --  If the first character in Image is a space, return the rest of Image

   function Pad_Number (Image : String; Digit_Count : Positive) return String;
   --  Return Strip_Number (Image) left-padded with 0 so that the result is
   --  Digit_Count long.

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

   -------------------
   -- Format_String --
   -------------------

   function Format_String
     (S : Unbounded_UTF8_String) return Unbounded_UTF8_String
   is
      Result : Unbounded_UTF8_String;
   begin
      Append (Result, """");
      for I in 1 .. Length (S) loop
         --  Forward printable ASCII bytes (except quotes and backslashes)
         --  and non-ASCII bytes, but emit escapes for quotes and control
         --  characters.
         declare
            Char : constant Character := Element (S, I);
         begin
            case Char is
               --  The only way to represent the following control characters
               --  is to use the unicode escape (\uXXXX).
               when ASCII.NUL .. ASCII.BEL
                  | ASCII.VT
                  | ASCII.SO .. ASCII.US
                  | ASCII.DEL
               =>
                  declare
                     use type Interfaces.Unsigned_8;

                     Byte : Interfaces.Unsigned_8 := Character'Pos (Char);
                     Repr : String (1 .. 6) := "\u0000";
                     I    : Natural range 3 .. 6 := 6;
                  begin
                     while Byte /= 0 loop
                        declare
                           Nibble : constant Interfaces.Unsigned_8 :=
                              Byte mod 16;
                           Digit  : Interfaces.Unsigned_8;
                        begin
                           if Nibble <= 9 then
                              Digit := Character'Pos ('0') + Nibble;
                           else
                              Digit := Character'Pos ('a') - 10 + Nibble;
                           end if;
                           Repr (I) := Character'Val (Digit);
                           Byte := Byte / 16;
                           I := I - 1;
                        end;
                     end loop;
                     Append (Result, Repr);
                  end;

               --  The following control characters have dedicated escape
               --  sequences:

               when ASCII.BS => Append (Result, "\b");
               when ASCII.HT => Append (Result, "\t");
               when ASCII.LF => Append (Result, "\n");
               when ASCII.FF => Append (Result, "\f");
               when ASCII.CR => Append (Result, "\r");

               --  Quotes and backslashes must be escaped

               when '"'      => Append (Result, "\""");
               when '\'      => Append (Result, "\\");

               --  All other bytes can be directly forwarded to the string
               --  literal.

               when ' ' | '!'
                  | '#' .. '['
                  | ']' .. '~'
                  | Character'Val (128) .. Character'Val(255)
               =>
                  Append (Result, Char);
            end case;
         end;
      end loop;
      Append (Result, """");
      return Result;
   end Format_String;

   ----------------
   -- Format_Key --
   ----------------

   function Format_Key
     (Key : Unbounded_UTF8_String) return Unbounded_UTF8_String is
   begin
      --  Determine if we need to quote Key, and if so, do it

      for I in 1 .. Length (Key) loop
         if Element (Key, I) not in
            '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '-' | '_'
         then
            return Format_String (Key);
         end if;
      end loop;

      if Length (Key) = 0 then
         return Format_String (Key);
      end if;

      --  Otherwise, we can return the key as-is (without quoting)

      return Key;
   end Format_Key;

   ----------------
   -- Append_Key --
   ----------------

   function Append_Key
     (Prefix, Suffix : Unbounded_UTF8_String) return Unbounded_UTF8_String
   is
      Result : Unbounded_UTF8_String := Prefix;
   begin
      if Length (Result) > 0 then
         Append (Result, ".");
      end if;
      Append (Result, Format_Key (Suffix));
      return Result;
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
               if (for all I in 1 .. P.Value.Length =>
                   P.Value.Item (I).Kind = TOML_Table)
               then
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
            Put (To_String (Format_Key (Pair.Key)));
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

   ------------------
   -- Strip_Number --
   ------------------

   function Strip_Number (Image : String) return String is
   begin
      if Image'Length > 0 and then Image (Image'First) = ' ' then
         return Image (Image'First + 1 .. Image'Last);
      else
         return Image;
      end if;
   end Strip_Number;

   ----------------
   -- Pad_Number --
   ----------------

   function Pad_Number (Image : String; Digit_Count : Positive) return String
   is
      Result : constant String := Strip_Number (Image);
   begin
      pragma Assert (Result'Length <= Digit_Count);
      return (Result'Length + 1 .. Digit_Count => '0') & Result;
   end Pad_Number;

   -----------------
   -- Dump_Inline --
   -----------------

   procedure Dump_Inline (Value : TOML_Value) is

      procedure Put (Datetime : TOML.Any_Local_Datetime);
      procedure Put (Date : TOML.Any_Local_Date);
      procedure Put (Time : TOML.Any_Local_Time);

      ---------
      -- Put --
      ---------

      procedure Put (Datetime : TOML.Any_Local_Datetime) is
      begin
         Put (Datetime.Date);
         Put ("T");
         Put (Datetime.Time);
      end Put;

      procedure Put (Date : TOML.Any_Local_Date) is
      begin
         Put (Pad_Number (Date.Year'Image, 4)
              & "-" & Pad_Number (Date.Month'Image, 2)
              & "-" & Pad_Number (Date.Day'Image, 2));
      end Put;

      procedure Put (Time : TOML.Any_Local_Time) is
      begin
         Put (Pad_Number (Time.Hour'Image, 2)
              & ":" & Pad_Number (Time.Minute'Image, 2)
              & ":" & Pad_Number (Time.Second'Image, 2)
              & "." & Pad_Number (Time.Millisecond'Image, 3));
      end Put;

   begin
      case Value.Kind is
         when TOML_Boolean =>
            Put ((if Value.As_Boolean then "true" else "false"));

         when TOML_Integer =>
            Put (Strip_Number (Any_Integer'Image (Value.As_Integer)));

         when TOML_Float =>
            declare
               V : constant Any_Float := Value.As_Float;
            begin
               case V.Kind is
                  when Regular =>
                     Put (Strip_Number (V.Value'Image));
                  when NaN | Infinity =>
                     Put (if V.Positive then "" else "-");
                     Put (if V.Kind = NaN then "nan" else "inf");
               end case;
            end;

         when TOML_String =>
            --  TODO: escape strings when needed

            Put (To_String (Format_String (Value.As_Unbounded_String)));

         when TOML_Offset_Datetime =>
            declare
               V               : constant Any_Offset_Datetime :=
                  Value.As_Offset_Datetime;
               Absolute_Offset : constant Any_Local_Offset :=
                 (if V.Offset < 0
                  then -V.Offset
                  else V.Offset);
               Hour_Offset     : constant Any_Local_Offset :=
                  Absolute_Offset / 60;
               Minute_Offset   : constant Any_Local_Offset :=
                  Absolute_Offset mod 60;
            begin
               Put (V.Datetime);
               if V.Offset < 0 or else V.Unknown_Offset then
                  Put ("-");
               else
                  Put ("+");
               end if;
               Put (Pad_Number (Hour_Offset'Image, 2)
                    & ":" & Pad_Number (Minute_Offset'Image, 2));
            end;

         when TOML_Local_Datetime =>
            Put (Value.As_Local_Datetime);

         when TOML_Local_Date =>
            Put (Value.As_Local_Date);

         when TOML_Local_Time =>
            Put (Value.As_Local_Time);

         when TOML_Array =>
            Put ("[" & ASCII.LF);
            for I in 1 .. Value.Length loop
               Dump_Inline (Value.Item (I));
               Put ("," & ASCII.LF);
            end loop;
            Put ("]");

         when TOML_Table =>
            Put ("{");
            declare
               First : Boolean := True;
            begin
               for E of Value.Iterate_On_Table loop
                  if not First then
                     Put (", ");
                  end if;
                  Put (To_String (Format_String (E.Key)));
                  Put (" = ");
                  Dump_Inline (E.Value);
                  First := False;
               end loop;
            end;
            Put ("}");
      end case;
   end Dump_Inline;

begin
   Dump_Toplevel_Table (Null_Unbounded_String, Value);
end TOML.Generic_Dump;
