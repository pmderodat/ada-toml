with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Interfaces;

package TOML is

   pragma Warnings (Off);
   use type Ada.Strings.Unbounded.Unbounded_String;
   pragma Warnings (On);

   subtype Unbounded_UTF8_String is Ada.Strings.Unbounded.Unbounded_String;

   type Any_Value_Kind is
     (TOML_Table,
      TOML_Array,
      TOML_String,
      TOML_Integer,
      TOML_Float,
      TOML_Boolean,
      TOML_Offset_Date_Time,
      TOML_Local_Date_Time,
      TOML_Local_Date,
      TOML_Local_Time);

   subtype Composite_Value_Kind is
      Any_Value_Kind range TOML_Table .. TOML_Array;

   subtype Atom_Value_Kind is
      Any_Value_Kind range TOML_String ..  TOML_Local_Time;

   type TOML_Value is new Ada.Finalization.Controlled with private;
   No_TOML_Value : constant TOML_Value;

   --  TODO: create TOML_Value subtypes for the various kinds

   type Any_Integer is new Interfaces.Integer_64;
   --  TOML supports any integer that can be encoded in a 64-bit signed
   --  integer.

   type Any_Float is new Interfaces.IEEE_Float_64;
   --  TOML advises to implement its float values as IEEE 754 binary64 values

   -----------------------
   -- Generic accessors --
   -----------------------

   function Is_Null (Value : TOML_Value) return Boolean;
   --  Return whether Value is a null reference

   function Is_Present (Value : TOML_Value) return Boolean
   is (not Value.Is_Null);

   function Kind (Value : TOML_Value) return Any_Value_Kind
      with Pre => Value.Is_Present;
   --  Return the kind of TOML node for the given Value

   function Clone (Value : TOML_Value) return TOML_Value
      with Pre => Value.Is_Present;
   --  Return a reference to a deep copy for Value

   --------------------
   -- Atom accessors --
   --------------------

   function As_Boolean (Value : TOML_Value) return Boolean
      with Pre => Value.Kind = TOML_Boolean;
   --  Return the boolean that Value represents

   function As_Integer (Value : TOML_Value) return Any_Integer
      with Pre => Value.Kind = TOML_Integer;
   --  Return the integer that Value represents

   function As_String (Value : TOML_Value) return String
      with Pre => Value.Kind = TOML_String;
   --  Return the string that Value represents

   function As_Unbounded_String
     (Value : TOML_Value) return Unbounded_UTF8_String
      with Pre => Value.Kind = TOML_String;
   --  Likewise, but return an unbounded string

   ---------------------
   -- Table accessors --
   ---------------------

   function Has (Value : TOML_Value; Key : String) return Boolean
      with Pre => Value.Kind = TOML_Table;
   --  Return whether Value contains an entry for the given Key

   function Has
     (Value : TOML_Value; Key : Unbounded_UTF8_String) return Boolean
      with Pre => Value.Kind = TOML_Table;
   --  Likewise, but take an unbounded string

   type Key_Array is array (Positive range <>) of Unbounded_UTF8_String;

   function Keys (Value : TOML_Value) return Key_Array
      with Pre => Value.Kind = TOML_Table;
   --  Return a list for all keys in the given table. Note that the result is
   --  sorted.

   function Get (Value : TOML_Value; Key : String) return TOML_Value
      with Pre => Value.Has (Key);
   --  Return the value for the entry in Value corresponding to Key

   function Get
     (Value : TOML_Value; Key : Unbounded_UTF8_String) return TOML_Value
      with Pre => Value.Has (Key);
   --  Likewise, but take an unbounded string

   function Get_Or_Null (Value : TOML_Value; Key : String) return TOML_Value
      with Pre => Value.Kind = TOML_Table;
   --  If there is an entry in the Value table, return its value. Return
   --  No_TOML_Value otherwise.

   function Get_Or_Null
     (Value : TOML_Value; Key : Unbounded_UTF8_String) return TOML_Value
      with Pre => Value.Kind = TOML_Table;
   --  Likewise, but take an unbounded string

   ---------------------
   -- Array accessors --
   ---------------------

   function Length (Value : TOML_Value) return Natural
      with Pre => Value.Kind = TOML_Array;
   --  Return the number of items in Value

   function Item_Kind_Set (Value : TOML_Value) return Boolean
      with Pre => Value.Kind = TOML_Array;
   --  Return whether the kind for array items in Value is determined

   function Item_Kind (Value : TOML_Value) return Any_Value_Kind
      with Pre => Value.Item_Kind_Set;
   --  Return the kind for array items in Value

   function Item_Kind_Matches
     (Value : TOML_Value; Item : TOML_Value) return Boolean
   is (not Value.Item_Kind_Set or else Value.Item_Kind = Item.Kind);
   --  Return whether Item's kind matches what the Value array expects

   function Item (Value : TOML_Value; Index : Positive) return TOML_Value
      with Pre  => Value.Kind = TOML_Array
                   and then Index <= Value.Length,
           Post => Item'Result.Kind = Value.Item_Kind;
   --  Return the item in Value at the given Index

   -------------------
   -- Atom creators --
   -------------------

   function Create_Boolean (Value : Boolean) return TOML_Value
      with Post => Create_Boolean'Result.Kind = TOML_Boolean
                   and then Create_Boolean'Result.As_Boolean = Value;
   --  Create a TOML boolean value

   function Create_Integer (Value : Any_Integer) return TOML_Value
      with Post => Create_Integer'Result.Kind = TOML_Integer
                   and then Create_Integer'Result.As_Integer = Value;
   --  Create a TOML integer value

   function Create_String (Value : String) return TOML_Value
      with Post => Create_String'Result.Kind = TOML_String
                   and then Create_String'Result.As_String = Value;
   --  Create a TOML string value. Value must be a valid UTF-8 string.

   function Create_String (Value : Unbounded_UTF8_String) return TOML_Value
      with Post => Create_String'Result.Kind = TOML_String
                   and then Create_String'Result.As_Unbounded_String = Value;
   --  Create a TOML string valeu

   ---------------------
   -- Table modifiers --
   ---------------------

   function Create_Table return TOML_Value
      with Post => Create_Table'Result.Kind = TOML_Table;
   --  Create an empty TOML table

   procedure Set (Value : TOML_Value; Key : String; Entry_Value : TOML_Value)
      with Pre => Value.Kind = TOML_Table;
   --  Create an entry in Value to bind Key to Entry_Value. If Value already
   --  has an entry for Key, replace it.

   procedure Set
     (Value       : TOML_Value;
      Key         : Unbounded_UTF8_String;
      Entry_Value : TOML_Value)
      with Pre => Value.Kind = TOML_Table;
   --  Likewise, but take an unbounded string

   procedure Set_Default
     (Value : TOML_Value; Key : String; Entry_Value : TOML_Value)
      with Pre => Value.Kind = TOML_Table;
   --  If Value has an entry for Key, do nothing. Otherwise, create an entry
   --  binding Key to Entry_Value.

   procedure Set_Default
     (Value       : TOML_Value;
      Key         : Unbounded_UTF8_String;
      Entry_Value : TOML_Value)
      with Pre => Value.Kind = TOML_Table;
   --  Likewise, but take an unbounded string

   ---------------------
   -- Array modifiers --
   ---------------------

   function Create_Array (Item_Kind : Any_Value_Kind) return TOML_Value
      with Post => Create_Array'Result.Kind = TOML_Array
                   and then Create_Array'Result.Item_Kind = Item_Kind;
   --  Create a TOML array to contain items of the given Item_Kind

   function Create_Array return TOML_Value
      with Post => Create_Array'Result.Kind = TOML_Array
                   and then not Create_Array'Result.Item_Kind_Set;
   --  Create a TOML array. Kind of array items is left undefined.

   procedure Set (Value : TOML_Value; Index : Positive; Item : TOML_Value)
      with Pre => Value.Kind = TOML_Array
                  and then Value.Item_Kind_Matches (Item)
                  and then Index <= Value.Length;
   --  Replace the Index'th item in Value with Item

   procedure Append (Value, Item : TOML_Value)
      with Pre => Value.Kind = TOML_Array
                  and then Value.Item_Kind_Matches (Item);
   --  Append Item to the Value array

   procedure Insert_Before
     (Value : TOML_Value; Index : Positive; Item : TOML_Value)
      with Pre => Value.Kind = TOML_Array
                  and then Value.Item_Kind_Matches (Item)
                  and then Index < Value.Length + 1;
   --  Insert Item before the Item'th element in the Value array

   ------------------
   -- Input/Output --
   ------------------

   type Source_Location is record
      Line, Column : Natural;
   end record;

   No_Location : constant Source_Location := (0, 0);

   --  Result of TOML document parsing. If the parsing was successful, contains
   --  the corresponding TOML value, otherwise, contains an error message that
   --  describes why parsing failed.

   type Read_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Message  : Unbounded_UTF8_String;
            Location : Source_Location;
         when True =>
            Value : TOML_Value;
      end case;
   end record;

   function Load_File (Filename : String) return Read_Result;
   --  Read Filename and parse its content as a TOML document

   function Load_String (Content : String) return Read_Result;
   --  Parse Content as a TOML document

   function Dump_As_String (Value : TOML_Value) return String
      with Pre => Value.Kind = TOML_Table;
   --  Serialize Value as a valid TOML document

   function Dump_As_Unbounded
     (Value : TOML_Value) return Unbounded_UTF8_String
      with Pre => Value.Kind = TOML_Table;
   --  Likewise, but return an unbounded string

   use all type Ada.Text_IO.File_Mode;

   procedure Dump_To_File
     (Value : TOML_Value; File : in out Ada.Text_IO.File_Type)
      with Pre => Value.Kind = TOML_Table
                  and then Ada.Text_IO.Mode (File) in Out_File | Append_File;
   --  Serialize Value and write the corresponding TOML document to File

private

   type TOML_Value_Record;
   type TOML_Value_Record_Access is access all TOML_Value_Record;

   type TOML_Value is new Ada.Finalization.Controlled with record
      Value : TOML_Value_Record_Access;
   end record;

   overriding procedure Adjust (Self : in out TOML_Value);
   overriding procedure Finalize (Self : in out TOML_Value);

   No_TOML_Value : constant TOML_Value := (Ada.Finalization.Controlled
                                           with Value => null);

end TOML;
