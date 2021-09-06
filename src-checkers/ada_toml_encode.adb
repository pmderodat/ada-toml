--  Test program. Read a valid toml-test compatible JSON description on the
--  standard input and emit a corresponding TOML document on the standard
--  output.

with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.JSON;

with TOML;
with TOML.Generic_Dump;

procedure Ada_TOML_Encode is

   use type Ada.Strings.Unbounded.Unbounded_String;
   use all type GNATCOLL.JSON.JSON_Value_Type;

   package US renames Ada.Strings.Unbounded;
   package IO renames Ada.Text_IO;
   package J renames GNATCOLL.JSON;

   type Stdout_Stream is null record;

   procedure Put (Stream : in out Stdout_Stream; Bytes : String);
   --  Callback for TOML.Generic_Dump

   function Interpret (Desc : J.JSON_Value) return TOML.TOML_Value;
   --  Interpret the given toml-test compatible JSON description (Value) and
   --  return the corresponding TOML value.

   type String_Array is array (Positive range <>) of US.Unbounded_String;

   procedure Sort_Strings is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => US.Unbounded_String,
      Array_Type   => String_Array,
      "<"          => US."<");

   function Sorted_Keys (Desc : J.JSON_Value) return String_Array
      with Pre => Desc.Kind = JSON_Object_Type;
   --  Return a sorted array for all keys in the Desc object

   ---------
   -- Put --
   ---------

   procedure Put (Stream : in out Stdout_Stream; Bytes : String) is
      pragma Unreferenced (Stream);
   begin
      IO.Put (Bytes);
   end Put;

   -----------------
   -- Sorted_Keys --
   -----------------

   function Sorted_Keys (Desc : J.JSON_Value) return String_Array is
      Count : Natural := 0;

      procedure Count_CB
        (Dummy_Name : J.UTF8_String; Dummy_Value : J.JSON_Value);

      --------------
      -- Count_CB --
      --------------

      procedure Count_CB
        (Dummy_Name : J.UTF8_String; Dummy_Value : J.JSON_Value) is
      begin
         Count := Count + 1;
      end Count_CB;
   begin
      Desc.Map_JSON_Object (Count_CB'Access);

      return Result : String_Array (1 .. Count) do
         declare
            I : Positive := Result'First;

            procedure Read_Entry
              (Name : J.UTF8_String; Dummy_Value : J.JSON_Value);

            ----------------
            -- Read_Entry --
            ----------------

            procedure Read_Entry
              (Name : J.UTF8_String; Dummy_Value : J.JSON_Value) is
            begin
               Result (I) := US.To_Unbounded_String (Name);
               I := I + 1;
            end Read_Entry;
         begin
            Desc.Map_JSON_Object (Read_Entry'Access);
            Sort_Strings (Result);
         end;
      end return;
   end Sorted_Keys;

   ---------------
   -- Interpret --
   ---------------

   function Interpret (Desc : J.JSON_Value) return TOML.TOML_Value is

      Time_Base_Length            : constant := 8;
      Time_Milli_Length           : constant := 4;
      Date_Length                 : constant := 10;
      Local_Datetime_Base_Length  : constant :=
         Date_Length + 1 + Time_Base_Length;
      Offset_Datetime_Base_Length : constant := Local_Datetime_Base_Length + 1;
      Offset_Full_Length          : constant := 6;

      function Decode_Offset_Datetime
        (S : String) return TOML.Any_Offset_Datetime;
      function Decode_Local_Datetime
        (S : String) return TOML.Any_Local_Datetime;
      function Decode_Date (S : String) return TOML.Any_Local_Date;
      function Decode_Time (S : String) return TOML.Any_Local_Time;

      ----------------------------
      -- Decode_Offset_Datetime --
      ----------------------------

      function Decode_Offset_Datetime
        (S : String) return TOML.Any_Offset_Datetime
      is
         use type TOML.Any_Local_Offset;

         pragma Assert (S'Length >= Offset_Datetime_Base_Length);

         Offset         : TOML.Any_Local_Offset;
         Unknown_Offset : Boolean;

         I    : constant Positive := S'First;
         Last : Positive := S'Last;
      begin
         if S (Last) = 'Z' then
            Last := Last - 1;
            Offset := 0;
            Unknown_Offset := False;
         else
            declare
               pragma Assert (S (Last - 2) = ':');

               Offset_Sign   : Character renames S (Last - 5);
               Hour_Offset   : String renames S (Last - 4 .. Last - 3);
               Minute_Offset : String renames S (Last - 1 .. Last);
            begin
               Offset := 60 * TOML.Any_Local_Offset'Value (Hour_Offset)
                         + TOML.Any_Local_Offset'Value (Minute_Offset);
               case Offset_Sign is
                  when '-'    => Offset := -Offset;
                  when '+'    => null;
                  when others => raise Program_Error;
               end case;
               Unknown_Offset := Offset = 0 and then Offset_Sign = '-';

               Last := Last - Offset_Full_Length;
            end;
         end if;

         declare
            Local_Datetime : constant TOML.Any_Local_Datetime :=
               Decode_Local_Datetime (S (I .. Last));
         begin
            return (Local_Datetime, Offset, Unknown_Offset);
         end;
      end Decode_Offset_Datetime;

      ---------------------------
      -- Decode_Local_Datetime --
      ---------------------------

      function Decode_Local_Datetime
        (S : String) return TOML.Any_Local_Datetime
      is
         I : constant Positive := S'First;

         pragma Assert (S'Length >= Local_Datetime_Base_Length);
         pragma Assert (S (I + Date_Length) = 'T');

         Date : constant TOML.Any_Local_Date :=
            Decode_Date (S (I .. I + Date_Length - 1));
         Time : constant TOML.Any_Local_Time :=
            Decode_Time (S (I + Date_Length + 1 .. S'Last));
      begin
         return (Date, Time);
      end Decode_Local_Datetime;

      -----------------
      -- Decode_Date --
      -----------------

      function Decode_Date (S : String) return TOML.Any_Local_Date is
         I : constant Positive := S'First;

         pragma Assert (S'Length = Date_Length);
         pragma Assert (S (I + 4) = '-');
         pragma Assert (S (I + 7) = '-');

         Year  : String renames S (I + 0 .. I + 3);
         Month : String renames S (I + 5 .. I + 6);
         Day   : String renames S (I + 8 .. I + 9);
      begin
         return (TOML.Any_Year'Value (Year),
                 TOML.Any_Month'Value (Month),
                 TOML.Any_Day'Value (Day));
      end Decode_Date;

      -----------------
      -- Decode_Time --
      -----------------

      function Decode_Time (S : String) return TOML.Any_Local_Time is
         I : constant Positive := S'First;

         pragma Assert (S'Length in Time_Base_Length
                                  | Time_Base_Length + Time_Milli_Length);
         pragma Assert (S (I + 2) = ':');
         pragma Assert (S (I + 5) = ':');

         Hour        : String renames S (I + 0 .. I + 1);
         Minute      : String renames S (I + 3 .. I + 4);
         Second      : String renames S (I + 6 .. I + 7);
         Millisecond : TOML.Any_Millisecond := 0;
      begin
         if S'Length /= Time_Base_Length then
            pragma Assert (S (I + Time_Base_Length) = '.');
            Millisecond := TOML.Any_Millisecond'Value
              (S (I + Time_Base_Length + 1 .. S'Last));
         end if;
         return (TOML.Any_Hour'Value (Hour),
                 TOML.Any_Minute'Value (Minute),
                 TOML.Any_Second'Value (Second),
                 Millisecond);
      end Decode_Time;

      Result : TOML.TOML_Value;
   begin
      case Desc.Kind is
         when JSON_Object_Type =>
            declare
               Keys : constant String_Array := Sorted_Keys (Desc);
            begin
               if Keys'Length = 2
                  and then Keys (1) = US.To_Unbounded_String ("type")
                  and then Keys (2) = US.To_Unbounded_String ("value")
               then
                  declare
                     T : constant String := Desc.Get ("type");
                     V : constant J.JSON_Value := Desc.Get ("value");
                  begin
                     if T = "string" then
                        declare
                           S : constant String := V.Get;
                        begin
                           Result := TOML.Create_String (S);
                        end;

                     elsif T = "float" then
                        declare
                           S        : constant String := V.Get;
                           I        : Positive := S'First;
                           Positive : Boolean := True;
                           Value    : TOML.Any_Float;
                        begin
                           if S (I) = '+' then
                              I := I + 1;
                           elsif S (I) = '-' then
                              Positive := False;
                              I := I + 1;
                           end if;

                           if S (I .. S'Last) = "nan" then
                              Value := (Kind     => TOML.NaN,
                                        Positive => Positive);
                           elsif S (I .. S'Last) = "inf" then
                              Value := (Kind     => TOML.Infinity,
                                        Positive => Positive);
                           else
                              declare
                                 use type TOML.Valid_Float;
                                 VF : TOML.Valid_Float :=
                                    TOML.Valid_Float'Value (S (I .. S'Last));
                              begin
                                 if not Positive then
                                    VF := -VF;
                                 end if;
                                 Value := (Kind => TOML.Regular, Value => VF);
                              end;
                           end if;

                           Result := TOML.Create_Float (Value);
                        end;

                     elsif T = "integer" then
                        declare
                           S : constant String := V.Get;
                        begin
                           Result :=
                              TOML.Create_Integer (TOML.Any_Integer'Value (S));
                        end;

                     elsif T = "bool" then
                        declare
                           S : constant String := V.Get;
                        begin
                           Result := TOML.Create_Boolean (Boolean'Value (S));
                        end;

                     elsif T = "datetime" then
                        Result := TOML.Create_Offset_Datetime
                          (Decode_Offset_Datetime (V.Get));

                     elsif T = "datetime-local" then
                        Result := TOML.Create_Local_Datetime
                          (Decode_Local_Datetime (V.Get));

                     elsif T = "date-local" then
                        Result := TOML.Create_Local_Date (Decode_Date (V.Get));

                     elsif T = "time-local" then
                        Result := TOML.Create_Local_Time (Decode_Time (V.Get));

                     elsif T = "array" then
                        Result := Interpret (V);

                     else
                        raise Program_Error with "unhandled value type: " & T;
                     end if;
                  end;

               else
                  Result := TOML.Create_Table;
                  for K of Keys loop
                     declare
                        Item : constant TOML.TOML_Value :=
                           Interpret (Desc.Get (US.To_String (K)));
                     begin
                        Result.Set (K, Item);
                     end;
                  end loop;
               end if;
            end;

         when JSON_Array_Type =>
            declare
               Elements : constant J.JSON_Array := Desc.Get;
            begin
               Result := TOML.Create_Array;
               for I in 1 .. J.Length (Elements) loop
                  Result.Append (Interpret (J.Get (Elements, I)));
               end loop;
            end;

         when others =>
            raise Program_Error;
      end case;

      return Result;
   end Interpret;

   procedure Dump is new TOML.Generic_Dump (Stdout_Stream, Put);

   Input       : US.Unbounded_String;
   Description : J.JSON_Value;
   Result      : TOML.TOML_Value;
   Stdout      : Stdout_Stream := (null record);
begin
   --  Read the stdin until end of file and store its content in Input

   loop
      begin
         declare
            Line : constant String := IO.Get_Line;
         begin
            US.Append (Input, Line);
         end;
      exception
         when IO.End_Error =>
            exit;
      end;
   end loop;

   --  Decode this input as JSON

   Description := J.Read (US.To_String (Input));

   --  Build the TOML document from the JSON description and output it on the
   --  standard output.

   Result := Interpret (Description);
   Dump (Stdout, Result);
end Ada_TOML_Encode;
