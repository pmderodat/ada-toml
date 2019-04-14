with Ada.Text_IO;

package TOML.File_IO is

   --  Subprograms to load/save TOML files

   use all type Ada.Text_IO.File_Mode;

   function Load_File (Filename : String) return Read_Result;
   --  Read Filename and parse its content as a TOML document

   procedure Dump_To_File
     (Value : TOML_Value; File : in out Ada.Text_IO.File_Type)
      with Pre => Value.Kind = TOML_Table
                  and then Ada.Text_IO.Mode (File) in Out_File | Append_File;
   --  Serialize Value and write the corresponding TOML document to File

end TOML.File_IO;
