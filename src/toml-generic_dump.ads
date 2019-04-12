generic
   type Output_Stream (<>) is limited private;
   --  Stream of bytes

   with procedure Put (Stream : in out Output_Stream; Bytes : String) is <>;
   --  Write all Bytes in Stream

procedure TOML.Generic_Dump
  (Stream : in out Output_Stream;
   Value  : TOML_Value)
   with Preelaborate, Pre => Value.Kind = TOML_Table;
--  Turn the given Value into a valid TOML document and write it to Stream
