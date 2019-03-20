with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with TOML.Generic_Dump;
with TOML.Generic_Parse;

package body TOML is

   use Ada.Strings.Unbounded;

   procedure Get
     (Stream : in out Ada.Text_IO.File_Type;
      EOF    : out Boolean;
      Byte   : out Character);
   --  Callback for Parse_File

   function Parse_File is new TOML.Generic_Parse
     (Input_Stream => Ada.Text_IO.File_Type,
      Get          => Get);

   procedure Dump_To_String is new TOML.Generic_Dump
     (Output_Stream => Unbounded_UTF8_String,
      Put           => Append);

   procedure Put_To_File (File : in out Ada.Text_IO.File_Type; Bytes : String);
   --  Callback for TOML.Generic_Dump

   procedure Dump_To_File is new TOML.Generic_Dump
     (Output_Stream => Ada.Text_IO.File_Type,
      Put           => Put_To_File);

   procedure Sort_Keys is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Unbounded_UTF8_String,
      Array_Type   => Key_Array);

   package TOML_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => TOML_Value,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package TOML_Vectors is new Ada.Containers.Vectors (Positive, TOML_Value);

   type TOML_Value_Record (Kind : Any_Value_Kind) is limited record
      Ref_Count : Natural;

      case Kind is
         when TOML_Table =>
            Map_Value : TOML_Maps.Map;

         when TOML_Array =>
            Item_Kind_Set : Boolean;
            --  Whether the kind for array items have been determined

            Item_Kind : Any_Value_Kind;
            --  Kind for all items in this array. Valid iff Item_Kind_Set is
            --  true.

            Array_Value : TOML_Vectors.Vector;
            --  List of values for all items

         when TOML_String =>
            String_Value : Unbounded_String;

         when TOML_Integer =>
            Integer_Value : Any_Integer;

         when TOML_Float =>
            Float_Value : Any_Float;

         when TOML_Boolean =>
            Boolean_Value : Boolean;

         when TOML_Offset_Date_Time => null;
         when TOML_Local_Date_Time => null;
         when TOML_Local_Date => null;
         when TOML_Local_Time => null;
      end case;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (TOML_Value_Record, TOML_Value_Record_Access);

   function Create_Value (Rec : TOML_Value_Record_Access) return TOML_Value;
   --  Wrap a value record in a value. This resets its ref-count to 1.

   function Create_Error
     (Message : String; Location : Source_Location) return Read_Result;
   --  Create an unsuccessful Read_Result value with the provided error
   --  information.

   procedure Set_Item_Kind (Value : TOML_Value; Item : TOML_Value)
      with Pre => Value.Kind = TOML_Array;
   --  If Value (an array) has its item kind set, do nothing. Otherwise, set it
   --  to Item's kind.

   ---------
   -- Get --
   ---------

   procedure Get
     (Stream : in out Ada.Text_IO.File_Type;
      EOF    : out Boolean;
      Byte   : out Character) is
   begin
      EOF := False;
      Ada.Text_IO.Get_Immediate (Stream, Byte);
   exception
      when Ada.Text_IO.End_Error =>
         EOF := True;
   end Get;

   -----------------
   -- Put_To_File --
   -----------------

   procedure Put_To_File (File : in out Ada.Text_IO.File_Type; Bytes : String)
   is
   begin
      Ada.Text_IO.Put (File, Bytes);
   end Put_To_File;

   ------------------
   -- Create_Value --
   ------------------

   function Create_Value (Rec : TOML_Value_Record_Access) return TOML_Value is
   begin
      return Result : TOML_Value do
         Rec.Ref_Count := 1;
         Result.Value := Rec;
      end return;
   end Create_Value;

   ------------------
   -- Create_Error --
   ------------------

   function Create_Error
     (Message : String; Location : Source_Location) return Read_Result is
   begin
      return
        (Success  => False,
         Message  => To_Unbounded_String (Message),
         Location => Location);
   end Create_Error;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Value : TOML_Value) return Boolean is
   begin
      return Value.Value = null;
   end Is_Null;

   ----------
   -- Kind --
   ----------

   function Kind (Value : TOML_Value) return Any_Value_Kind is
   begin
      return Value.Value.Kind;
   end Kind;

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Value : TOML_Value) return Boolean is
   begin
      return Value.Value.Boolean_Value;
   end As_Boolean;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Value : TOML_Value) return Any_Integer is
   begin
      return Value.Value.Integer_Value;
   end As_Integer;

   ---------------
   -- As_String --
   ---------------

   function As_String (Value : TOML_Value) return String is
   begin
      return To_String (Value.As_Unbounded_String);
   end As_String;

   -------------------------
   -- As_Unbounded_String --
   -------------------------

   function As_Unbounded_String
     (Value : TOML_Value) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Value.Value.String_Value;
   end As_Unbounded_String;

   ---------
   -- Has --
   ---------

   function Has (Value : TOML_Value; Key : String) return Boolean is
   begin
      return Value.Has (To_Unbounded_String (Key));
   end Has;

   ---------
   -- Has --
   ---------

   function Has
     (Value : TOML_Value; Key : Unbounded_UTF8_String) return Boolean is
   begin
      return Value.Value.Map_Value.Contains (Key);
   end Has;

   ----------
   -- Keys --
   ----------

   function Keys (Value : TOML_Value) return Key_Array is
      use TOML_Maps;
      Map : TOML_Maps.Map renames Value.Value.Map_Value;
      I   : Positive := 1;
   begin
      return Result : Key_Array (1 .. Natural (Map.Length)) do
         for Position in Map.Iterate loop
            Result (I) := Key (Position);
            I := I + 1;
         end loop;
         Sort_Keys (Result);
      end return;
   end Keys;

   ---------
   -- Get --
   ---------

   function Get (Value : TOML_Value; Key : String) return TOML_Value is
   begin
      return Value.Get (To_Unbounded_String (Key));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Value : TOML_Value; Key : Unbounded_UTF8_String) return TOML_Value is
   begin
      return Value.Value.Map_Value.Element (Key);
   end Get;

   -----------------
   -- Get_Or_Null --
   -----------------

   function Get_Or_Null (Value : TOML_Value; Key : String) return TOML_Value
   is
   begin
      return Value.Get_Or_Null (To_Unbounded_String (Key));
   end Get_Or_Null;

   -----------------
   -- Get_Or_Null --
   -----------------

   function Get_Or_Null
     (Value : TOML_Value; Key : Unbounded_UTF8_String) return TOML_Value
   is
      use TOML_Maps;
      Position : constant Cursor := Value.Value.Map_Value.Find (Key);
   begin
      return (if Has_Element (Position)
              then Element (Position)
              else No_TOML_Value);
   end Get_Or_Null;

   ------------
   -- Length --
   ------------

   function Length (Value : TOML_Value) return Natural is
   begin
      return Natural (Value.Value.Array_Value.Length);
   end Length;

   -------------------
   -- Item_Kind_Set --
   -------------------

   function Item_Kind_Set (Value : TOML_Value) return Boolean is
   begin
      return Value.Value.Item_Kind_Set;
   end Item_Kind_Set;

   ---------------
   -- Item_Kind --
   ---------------

   function Item_Kind (Value : TOML_Value) return Any_Value_Kind is
   begin
      return Value.Value.Item_Kind;
   end Item_Kind;

   ----------
   -- Item --
   ----------

   function Item (Value : TOML_Value; Index : Positive) return TOML_Value is
   begin
      return Value.Value.Array_Value.Element (Index);
   end Item;

   --------------------
   -- Create_Boolean --
   --------------------

   function Create_Boolean (Value : Boolean) return TOML_Value is
   begin
      return Create_Value (new TOML_Value_Record'
        (Kind => TOML_Boolean, Ref_Count => 1, Boolean_Value => Value));
   end Create_Boolean;

   --------------------
   -- Create_Integer --
   --------------------

   function Create_Integer (Value : Any_Integer) return TOML_Value is
   begin
      return Create_Value (new TOML_Value_Record'
        (Kind => TOML_Integer, Ref_Count => 1, Integer_Value => Value));
   end Create_Integer;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Value : String) return TOML_Value is
   begin
      return Create_String (To_Unbounded_String (Value));
   end Create_String;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Value : Unbounded_UTF8_String) return TOML_Value is
   begin
      return Create_Value (new TOML_Value_Record'
        (Kind         => TOML_String,
         Ref_Count    => 1,
         String_Value => Value));
   end Create_String;

   ------------------
   -- Create_Table --
   ------------------

   function Create_Table return TOML_Value is
   begin
      return Create_Value (new TOML_Value_Record'
        (Kind => TOML_Table, Ref_Count => 1, Map_Value => <>));
   end Create_Table;

   ---------
   -- Set --
   ---------

   procedure Set (Value : TOML_Value; Key : String; Entry_Value : TOML_Value)
   is
   begin
      Value.Set (To_Unbounded_String (Key), Entry_Value);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Value       : TOML_Value;
      Key         : Unbounded_UTF8_String;
      Entry_Value : TOML_Value)
   is
   begin
      Value.Value.Map_Value.Include (Key, Entry_Value);
   end Set;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Value : TOML_Value; Key : String; Entry_Value : TOML_Value)
   is
   begin
      Value.Set_Default (To_Unbounded_String (Key), Entry_Value);
   end Set_Default;

   -----------------
   -- Set_Default --
   -----------------

   procedure Set_Default
     (Value       : TOML_Value;
      Key         : Unbounded_UTF8_String;
      Entry_Value : TOML_Value)
   is
      use TOML_Maps;
      Dummy_Position : Cursor;
      Dummy_Inserted : Boolean;
   begin
      Value.Value.Map_Value.Insert
        (Key, Entry_Value, Dummy_Position, Dummy_Inserted);
   end Set_Default;

   ------------------
   -- Create_Array --
   ------------------

   function Create_Array (Item_Kind : Any_Value_Kind) return TOML_Value is
   begin
      return Create_Value (new TOML_Value_Record'
        (Kind          => TOML_Array,
         Ref_Count     => 1,
         Item_Kind_Set => True,
         Item_Kind     => Item_Kind,
         Array_Value   => <>));
   end Create_Array;

   ------------------
   -- Create_Array --
   ------------------

   function Create_Array return TOML_Value is
   begin
      return Create_Value (new TOML_Value_Record'
        (Kind          => TOML_Array,
         Ref_Count     => 1,
         Item_Kind_Set => False,
         Item_Kind     => <>,
         Array_Value   => <>));
   end Create_Array;

   -------------------
   -- Set_Item_Kind --
   -------------------

   procedure Set_Item_Kind (Value : TOML_Value; Item : TOML_Value) is
   begin
      if not Value.Item_Kind_Set then
         Value.Value.Item_Kind_Set := True;
         Value.Value.Item_Kind := Item.Kind;
      end if;
   end Set_Item_Kind;

   ---------
   -- Set --
   ---------

   procedure Set (Value : TOML_Value; Index : Positive; Item : TOML_Value) is
   begin
      Set_Item_Kind (Value, Item);
      Value.Value.Array_Value (Index) := Item;
   end Set;

   ------------
   -- Append --
   ------------

   procedure Append (Value, Item : TOML_Value) is
   begin
      Set_Item_Kind (Value, Item);
      Value.Value.Array_Value.Append (Item);
   end Append;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
     (Value : TOML_Value; Index : Positive; Item : TOML_Value)
   is
   begin
      Set_Item_Kind (Value, Item);
      Value.Value.Array_Value.Insert (Index, Item);
   end Insert_Before;

   ---------------
   -- Load_File --
   ---------------

   function Load_File (Filename : String) return Read_Result is
      use Ada.Exceptions, Ada.Text_IO;

      File : File_Type;
   begin
      begin
         Open (File, In_File, Filename);
      exception
         when Exc : Name_Error | Use_Error =>
            return Create_Error
              ("cannot open " & Filename & ": " & Exception_Message (Exc),
               No_Location);
      end;

      return Result : constant Read_Result := Parse_File (File) do
         Close (File);
      end return;
   end Load_File;

   -----------------
   -- Load_String --
   -----------------

   function Load_String (Content : String) return Read_Result is
      type Input_Stream is record
         Next_Character : Positive;
         --  Index of the next character in Content that Get must return
      end record;

      procedure Get
        (Stream : in out Input_Stream;
         EOF    : out Boolean;
         Byte   : out Character);
      --  Callback for Parse_String

      ---------
      -- Get --
      ---------

      procedure Get
        (Stream : in out Input_Stream;
         EOF    : out Boolean;
         Byte   : out Character) is
      begin
         if Stream.Next_Character > Content'Length then
            EOF := True;
         else
            EOF := False;
            Byte := Content (Stream.Next_Character);
            Stream.Next_Character := Stream.Next_Character + 1;
         end if;
      end Get;

      function Parse_String is new TOML.Generic_Parse (Input_Stream, Get);

      Stream : Input_Stream := (Next_Character => Content'First);
   begin
      return Parse_String (Stream);
   end Load_String;

   --------------------
   -- Dump_As_String --
   --------------------

   function Dump_As_String (Value : TOML_Value) return String is
   begin
      return To_String (Dump_As_Unbounded (Value));
   end Dump_As_String;

   -----------------------
   -- Dump_As_Unbounded --
   -----------------------

   function Dump_As_Unbounded
     (Value : TOML_Value) return Unbounded_UTF8_String is
   begin
      return Result : Unbounded_UTF8_String do
         Dump_To_String (Result, Value);
      end return;
   end Dump_As_Unbounded;

   ------------------
   -- Dump_To_File --
   ------------------

   procedure Dump_To_File
     (Value : TOML_Value; File : in out Ada.Text_IO.File_Type) is
   begin
      Dump_To_File (File, Value);
   end Dump_To_File;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out TOML_Value) is
   begin
      if Self.Value = null then
         return;
      end if;

      Self.Value.Ref_Count := Self.Value.Ref_Count + 1;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out TOML_Value) is
   begin
      if Self.Value = null then
         return;
      end if;

      declare
         V : TOML_Value_Record renames Self.Value.all;
      begin
         --  Decrement the ref-count. If no-one references V anymore,
         --  deallocate it.

         V.Ref_Count := V.Ref_Count - 1;
         if V.Ref_Count > 0 then
            return;
         end if;
      end;

      Free (Self.Value);
   end Finalize;

end TOML;
