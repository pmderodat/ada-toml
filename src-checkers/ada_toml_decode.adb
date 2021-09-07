--  Test program. Read bytes on the standard input as a TOML document.
--
--  If it's a valid TOML document, parse it and emit on the standard output a
--  JSON representation of it.
--
--  If it's not a valid TOML document, print an error message on the standard
--  output.

with Ada.Command_Line;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Interfaces;

with TOML;
with TOML.Generic_Parse;

procedure Ada_TOML_Decode is

   use type TOML.Any_Value_Kind;

   package Cmd renames Ada.Command_Line;
   package IO renames Ada.Text_IO;

   type Stdin_Stream is null record;

   procedure Get
     (Stream : in out Stdin_Stream; EOF : out Boolean; Byte : out Character);
   --  Callback for TOML.Generic_Parse

   subtype Wrapped_Kind is TOML.Any_Value_Kind
      with Static_Predicate =>
         Wrapped_Kind in TOML.TOML_Array .. TOML.TOML_Boolean
                       | TOML.TOML_Offset_Datetime | TOML.TOML_Local_Datetime
                       | TOML.TOML_Local_Date | TOML.TOML_Local_Time
                       | TOML.TOML_Float;

   function Kind_Name (Kind : Wrapped_Kind) return String;
   --  Return the name expected in the JSON output for the given kind

   function Strip_Number (Image : String) return String;
   --  If the first character in Image is a space, return the rest of Image

   function Pad_Number (Image : String; Digit_Count : Positive) return String;
   --  Return Strip_Number (Image) left-padded with 0 so that the result is
   --  Digit_Count long.

   procedure Dump_String (Value : TOML.Unbounded_UTF8_String);
   --  Dump the given string as a JSON string literal

   procedure Dump_Array (Value : TOML.TOML_Value)
      with Pre => TOML."=" (Value.Kind, TOML.TOML_Array);
   --  Dump the given TOML array as a JSON array

   procedure Dump (Value : TOML.TOML_Value);
   --  Dump the given TOML value using the expected JSON output format.
   --  Toplevel must be true for the root table, root table children and table
   --  arrays.

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Kind : Wrapped_Kind) return String is
   begin
      return (case Kind is
              when TOML.TOML_Array           => "array",
              when TOML.TOML_String          => "string",
              when TOML.TOML_Integer         => "integer",
              when TOML.TOML_Float           => "float",
              when TOML.TOML_Boolean         => "bool",
              when TOML.TOML_Offset_Datetime => "datetime",
              when TOML.TOML_Local_Datetime  => "datetime-local",
              when TOML.TOML_Local_Date      => "date-local",
              when TOML.TOML_Local_Time      => "time-local");
   end Kind_Name;

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
   -- Dump_String --
   -----------------

   procedure Dump_String (Value : TOML.Unbounded_UTF8_String) is
      use Ada.Strings.Unbounded;
   begin
      IO.Put ("""");
      declare
         S : constant Wide_Wide_String :=
            Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
              (To_String (Value));
      begin
         for C of S loop
            if C in '"' | '\' then
               IO.Put ("\" & Character'Val (Wide_Wide_Character'Pos (C)));
            elsif C in ' ' .. '~' then
               IO.Put ((1 => Character'Val (Wide_Wide_Character'Pos (C))));
            else
               declare
                  use type Interfaces.Unsigned_32;

                  Codepoint    : Interfaces.Unsigned_32 :=
                     Wide_Wide_Character'Pos (C);
                  Digits_Count : constant Positive :=
                    (if Codepoint <= 16#FFFF# then 4 else 8);
                  CP_Digits    : String (1 .. Digits_Count);
               begin
                  if Digits_Count = 4 then
                     IO.Put ("\u");
                  else
                     IO.Put ("\U");
                  end if;
                  for D of reverse CP_Digits loop
                     declare
                        subtype Hex_Digit is
                           Interfaces.Unsigned_32 range 0 ..  15;
                        Digit : constant Hex_Digit := Codepoint mod 16;
                     begin
                        case Digit is
                           when 0 .. 9 =>
                              D := Character'Val (Character'Pos ('0') + Digit);
                           when 10 .. 15 =>
                              D := Character'Val
                                (Character'Pos ('A') + Digit - 10);
                        end case;
                        Codepoint := Codepoint / 16;
                     end;
                  end loop;
                  IO.Put (CP_Digits);
               end;
            end if;
         end loop;
      end;
      IO.Put ("""");
   end Dump_String;

   ----------------
   -- Dump_Array --
   ----------------

   procedure Dump_Array (Value : TOML.TOML_Value) is
   begin
      IO.Put_Line ("[");
      for I in 1 .. Value.Length loop
         if I > 1 then
            IO.Put_Line (",");
         end if;
         Dump (Value.Item (I));
      end loop;
      IO.Put_Line ("]");
   end Dump_Array;

   ----------
   -- Dump --
   ----------

   procedure Dump (Value : TOML.TOML_Value) is
      use all type TOML.Any_Value_Kind;

      procedure Put (Datetime : TOML.Any_Local_Datetime);
      procedure Put (Date : TOML.Any_Local_Date);
      procedure Put (Time : TOML.Any_Local_Time);

      ---------
      -- Put --
      ---------

      procedure Put (Datetime : TOML.Any_Local_Datetime) is
      begin
         Put (Datetime.Date);
         IO.Put ("T");
         Put (Datetime.Time);
      end Put;

      procedure Put (Date : TOML.Any_Local_Date) is
      begin
         IO.Put (Pad_Number (Date.Year'Image, 4)
                 & "-" & Pad_Number (Date.Month'Image, 2)
                 & "-" & Pad_Number (Date.Day'Image, 2));
      end Put;

      procedure Put (Time : TOML.Any_Local_Time) is
         use type TOML.Any_Millisecond;
      begin
         IO.Put (Pad_Number (Time.Hour'Image, 2)
                 & ":" & Pad_Number (Time.Minute'Image, 2)
                 & ":" & Pad_Number (Time.Second'Image, 2));
         if Time.Millisecond /= 0 then
            IO.Put ("." & Pad_Number (Time.Millisecond'Image, 3));
         end if;
      end Put;

   begin
      if Value.Kind = TOML_Table then
         IO.Put_Line ("{");
         declare
            Keys : constant TOML.Key_Array := Value.Keys;
         begin
            for I in Keys'Range loop
               if I > Keys'First then
                  IO.Put_Line (",");
               end if;

               Dump_String (Keys (I));
               IO.Put_Line (":");
               Dump (Value.Get (Keys (I)));
            end loop;
         end;
         IO.Put_Line ("}");

      elsif Value.Kind = TOML.TOML_Array
            and then (for all I in 1 .. Value.Length =>
                      Value.Item (I).Kind = TOML_Table)
      then
         Dump_Array (Value);

      else
         IO.Put_Line
           ("{""type"": """ & Kind_Name (Value.Kind) & """, ""value"":");
         case Wrapped_Kind (Value.Kind) is
            when TOML_Array =>
               Dump_Array (Value);

            when TOML_String =>
               Dump_String (Value.As_Unbounded_String);
               IO.New_Line;

            when TOML_Integer =>
               IO.Put_Line ("""" & Strip_Number (Value.As_Integer'Image)
                            & """");

            when TOML_Float =>
               declare
                  V : constant TOML.Any_Float := Value.As_Float;
               begin
                  IO.Put ("""");
                  case V.Kind is
                     when TOML.Regular =>
                        IO.Put (Strip_Number (V.Value'Image));
                     when TOML.NaN =>
                        IO.Put (if V.Positive then "+" else "-");
                        IO.Put ("nan");
                     when TOML.Infinity =>
                        IO.Put (if V.Positive then "+" else "-");
                        IO.Put ("inf");
                  end case;
                  IO.Put_Line ("""");
               end;

            when TOML_Boolean =>
               if Value.As_Boolean then
                  IO.Put_Line ("""true""");
               else
                  IO.Put_Line ("""false""");
               end if;

            when TOML_Offset_Datetime =>
               declare
                  use type TOML.Any_Local_Offset;

                  V               : constant TOML.Any_Offset_Datetime :=
                     Value.As_Offset_Datetime;
                  Absolute_Offset : constant TOML.Any_Local_Offset :=
                    (if V.Offset < 0
                     then -V.Offset
                     else V.Offset);
                  Hour_Offset     : constant TOML.Any_Local_Offset :=
                     Absolute_Offset / 60;
                  Minute_Offset   : constant TOML.Any_Local_Offset :=
                     Absolute_Offset mod 60;
               begin
                  IO.Put ("""");
                  Put (V.Datetime);
                  if V.Offset = 0 and then not V.Unknown_Offset then
                     IO.Put ("Z");
                  else
                     if V.Offset <= 0 then
                        IO.Put ("-");
                     else
                        IO.Put ("+");
                     end if;
                     IO.Put (Pad_Number (Hour_Offset'Image, 2)
                             & ":" & Pad_Number (Minute_Offset'Image, 2));
                  end if;
                  IO.Put_Line ("""");
               end;

            when TOML_Local_Datetime =>
               IO.Put ("""");
               Put (Value.As_Local_Datetime);
               IO.Put_Line ("""");

            when TOML_Local_Date =>
               IO.Put ("""");
               Put (Value.As_Local_Date);
               IO.Put_Line ("""");

            when TOML_Local_Time =>
               IO.Put ("""");
               Put (Value.As_Local_Time);
               IO.Put_Line ("""");
         end case;
         IO.Put_Line ("}");
      end if;
   end Dump;

   ---------
   -- Get --
   ---------

   procedure Get
     (Stream : in out Stdin_Stream; EOF : out Boolean; Byte : out Character)
   is
      pragma Unreferenced (Stream);
      Available : Boolean;
   begin
      IO.Get_Immediate (Byte, Available);
      EOF := not Available;
   exception
      when IO.End_Error =>
         EOF := True;
   end Get;

   function Parse_File is new TOML.Generic_Parse (Stdin_Stream, Get);

   Stdin  : Stdin_Stream := (null record);
   Result : constant TOML.Read_Result := Parse_File (Stdin);
begin
   if Result.Success then
      Dump (Result.Value);
   else
      IO.Put_Line (TOML.Format_Error (Result));
      Cmd.Set_Exit_Status (Cmd.Failure);
   end if;
end Ada_TOML_Decode;
