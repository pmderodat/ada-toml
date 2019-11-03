with Ada.Text_IO;

with TOML;

procedure Main is
   procedure Assert (Predicate : Boolean; Message : String);
   --  If Predicate is false, emit the given error Message

   procedure Assert_Eq (Left, Right : TOML.TOML_Value; Message : String);
   --  Shortcut for Assert (TOML.Equals (Left, Right), Message)

   procedure Assert_Ne (Left, Right : TOML.TOML_Value; Message : String);
   --  Shortcut for Assert (not TOML.Equals (Left, Right), Message)

   ------------
   -- Assert --
   ------------

   procedure Assert (Predicate : Boolean; Message : String) is
   begin
      if not Predicate then
         Ada.Text_IO.Put_Line ("Test failed: " & Message);
      end if;
   end Assert;

   ---------------
   -- Assert_Eq --
   ---------------

   procedure Assert_Eq (Left, Right : TOML.TOML_Value; Message : String) is
   begin
      Assert (TOML.Equals (Left, Right), Message);
   end Assert_Eq;

   ---------------
   -- Assert_Ne --
   ---------------

   procedure Assert_Ne (Left, Right : TOML.TOML_Value; Message : String) is
   begin
      Assert (not TOML.Equals (Left, Right), Message);
   end Assert_Ne;

   use TOML;

begin
   Assert_Ne
     (Create_String ("ABCD"), Create_Integer (0), "different kinds");

   Assert_Eq
     (Create_Boolean (True), Create_Boolean (True), "same boolean");
   Assert_Ne
     (Create_Boolean (True), Create_Boolean (False), "different boolean");

   Assert_Eq
     (Create_Integer (0), Create_Integer (0), "same integer");
   Assert_Ne
     (Create_Integer (0), Create_Integer (1), "different integer");

   Assert_Eq
     (Create_String ("ABCD"), Create_String ("ABCD"), "same string");
   Assert_Ne
     (Create_String ("ABCD"), Create_String ("ABCDE"), "different string");

   declare
      Empty_1 : constant TOML_Value := Create_Array;
      Empty_2 : constant TOML_Value := Create_Array;

      One_Int_1  : constant TOML_Value := Create_Array;
      One_Int_2  : constant TOML_Value := Create_Array;
      Two_Ints   : constant TOML_Value := Create_Array;
      One_String : constant TOML_Value := Create_Array;
   begin
      One_Int_1.Append (Create_Integer (0));
      One_Int_2.Append (Create_Integer (0));
      Two_Ints.Append (Create_Integer (0));
      Two_Ints.Append (Create_Integer (1));
      One_String.Append (Create_String ("ABCD"));

      Assert_Eq (Empty_1, Empty_2, "empty arrays");
      Assert_Ne (Empty_1, One_Int_1, "different array length");

      Assert_Eq (One_Int_1, One_Int_2, "same int array");
      Assert_Ne (One_Int_1, Two_Ints, "different int array");
      Assert_Ne (One_Int_1, One_String, "different array item kind");
   end;

   declare
      Empty_1 : constant TOML_Value := Create_Table;
      Empty_2 : constant TOML_Value := Create_Table;

      Same_Key_1 : constant TOML_Value := Create_Table;
      Same_Key_2 : constant TOML_Value := Create_Table;
      Same_Key_3 : constant TOML_Value := Create_Table;

      Different_Key : constant TOML_Value := Create_Table;
   begin
      Same_Key_1.Set ("key", Create_Boolean (True));
      Same_Key_2.Set ("key", Create_Boolean (True));
      Same_Key_3.Set ("key", Create_Boolean (False));
      Different_Key.Set ("KEY", Create_Boolean (True));

      Assert_Eq (Empty_1, Empty_2, "empty tables");
      Assert_Ne (Empty_1, Same_Key_1, "different table size");

      Assert_Eq (Same_Key_1, Same_Key_2, "same table content");
      Assert_Ne (Same_Key_1, Same_Key_3, "different member value");

      Assert_Ne (Same_Key_1, Different_Key, "different key");
   end;

   Ada.Text_IO.Put_Line ("Done");
end Main;
