with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces;

function TOML.Generic_Parse
  (Stream : in out Input_Stream) return TOML.Read_Result
is

   ----------------------------
   -- Lexical analysis state --
   ----------------------------

   WW_Backspace       : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (Character'Pos (ASCII.BS));
   WW_Linefeed        : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (Character'Pos (ASCII.LF));
   WW_Form_Feed       : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (Character'Pos (ASCII.FF));
   WW_Carriage_Return : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (Character'Pos (ASCII.CR));
   WW_Tab             : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (Character'Pos (ASCII.HT));

   subtype WW_Control_Characters is Wide_Wide_Character
      with Static_Predicate => WW_Control_Characters in
            Wide_Wide_Character'Val (16#00#)
         .. Wide_Wide_Character'Val (16#1F#)
          | Wide_Wide_Character'Val (16#7F#);

   subtype WW_Non_ASCII is Wide_Wide_Character
      range Wide_Wide_Character'Val (16#80#) .. Wide_Wide_Character'Last;

   subtype WW_Surrogates is Wide_Wide_Character
      range Wide_Wide_Character'Val (16#D800#)
         .. Wide_Wide_Character'Val (16#DFFF#);

   type Codepoint_Buffer_Type (EOF : Boolean := False) is record
      To_Reemit : Boolean;
      --  If true, the next call to Read_Codepoint should do nothing, except
      --  resetting this to component to false.

      Location : Source_Location;

      case EOF is
         when True => null;
         when False =>
            Codepoint : Wide_Wide_Character;
      end case;
   end record;
   Codepoint_Buffer : Codepoint_Buffer_Type :=
     (EOF       => False,
      To_Reemit => False,
      Location  => No_Location,
      Codepoint => <>);
   --  Buffer used to temporarily store information read from Stream

   type Any_String_Format is
     (Bare_Key,
      Basic_String,
      Multiline_Basic_String,
      Literal_String,
      Multiline_Literal_String);
   subtype Valid_Key_Format is Any_String_Format with Static_Predicate =>
      Valid_Key_Format in Bare_Key | Basic_String | Literal_String;

   type Token_Kind is
     (Newline, Equal, Dot, Comma,

      Curly_Bracket_Open, Curly_Bracket_Close,
      Square_Bracket_Open, Square_Bracket_Close,
      Double_Square_Bracket_Open, Double_Square_Bracket_Close,

      Boolean_Literal, Integer_Literal, Float_Literal, String_Literal,
      Offset_Datetime_Literal, Local_Datetime_Literal,
      Local_Date_Literal, Local_Time_Literal);

   subtype No_Text_Token is Token_Kind range
      Newline ..  Double_Square_Bracket_Close;
   --  Tokens whose text does not matter: the kind itself holds enough
   --  information to describe the token.

   type Any_Token (Kind : Token_Kind := Token_Kind'First) is record
      case Kind is
         when No_Text_Token          => null;
         when Boolean_Literal        => Boolean_Value : Boolean;
         when Integer_Literal        => Integer_Value : Any_Integer;
         when Float_Literal          => Float_Value   : Any_Float;
         when String_Literal         =>
            String_Value  : Unbounded_UTF8_String;
            String_Format : Any_String_Format;
         when Offset_Datetime_Literal =>
            Offset_Datetime_Value : Any_Offset_Datetime;
         when Local_Datetime_Literal =>
            Local_Datetime_Value : Any_Local_Datetime;
         when Local_Date_Literal     => Local_Date_Value : Any_Local_Date;
         when Local_Time_Literal     => Local_Time_Value : Any_Local_Time;
      end case;
   end record;

   True_Keyword  : constant Any_Token :=
     (Kind => Boolean_Literal, Boolean_Value => True);
   False_Keyword : constant Any_Token :=
     (Kind => Boolean_Literal, Boolean_Value => False);

   type Token_Buffer_Type (EOF : Boolean := False) is record
      To_Reemit : Boolean;
      --  If true, the next call to Read_Token should do nothing, except
      --  resetting this to component to false.

      case EOF is
         when True => null;
         when False =>
            Token    : Any_Token;
            Location : Source_Location;
      end case;
   end record;
   Token_Buffer : Token_Buffer_Type :=
     (EOF => False, To_Reemit => False, Location => No_Location, Token => <>);

   ------------------------------
   -- Syntactic analysis state --
   ------------------------------

   Root_Table : constant TOML_Value := Create_Table;
   Result     : Read_Result := (Success => True, Value => Root_Table);

   Current_Table : TOML_Value := Root_Table;
   --  Table that the next key/value pair node will update

   function Create_Error
     (Message  : String;
      Location : Source_Location := No_Location) return Boolean
      with Post => not Create_Error'Result;
   --  Put in Result an unsuccessful Read_Result value with the provided error
   --  information. If no source location is passed, use
   --  Codepoint_Buffer.Location.
   --
   --  This is a function that always return False for convenience in parsing
   --  helpers, which are all functions that return False on error.

   function Create_Lexing_Error
     (Message : String := "invalid token") return Boolean
      with Post => not Create_Lexing_Error'Result;
   --  Like Create_Error, but use Token_Buffer.Location as the source location
   --  for the error.

   function Create_Syntax_Error
     (Message : String := "invalid syntax") return Boolean
      with Post => not Create_Syntax_Error'Result;
   --  Like Create_Error, but use Token_Buffer.Location as the source location
   --  for the error.

   -----------------------
   -- Codepoint reading --
   -----------------------

   function Read_Codepoint return Boolean
      with Pre => not Codepoint_Buffer.EOF or else Codepoint_Buffer.To_Reemit;
   --  Read a UTF-8 codepoint from Stream. If EOF is reached or a codepoint
   --  could be read, return True and update Codepoint_Buffer accordingly
   --  (location included). Otherwise, return False and put an error in Result.
   --
   --  Note that this automatically discards carriage returns codepoints when
   --  they are immediately followed by a linefeed. When they are not followed
   --  by a linefeed, this creates an error.

   procedure Reemit_Codepoint
      with Pre => not Codepoint_Buffer.To_Reemit;
   --  Re-emit the same codepoint the next time that Read_Codepoint is called

   --------------------
   -- Token decoding --
   --------------------

   function Read_Token (Key_Expected : Boolean) return Boolean
      with Pre => not Token_Buffer.EOF or else Token_Buffer.To_Reemit;
   --  Read a token from Stream. If EOF is reached or a token could be read,
   --  return True and update Token_Buffer accordingly (location included).
   --  Otherwise, return False and put an error in Result.
   --
   --  If Key_Expected is true:
   --
   --  * parse tokens that could be integer literals as string literals;
   --  * parse "[["/"]]" as double square bracket tokens instead of two
   --    consecutive square bracket tokens.

   procedure Reemit_Token
      with Pre => not Token_Buffer.To_Reemit;
   --  Re-emit the same token the next time that Read_Token is called

   procedure Append_As_UTF8 (Codepoint : Wide_Wide_Character)
      with Pre => not Token_Buffer.EOF
                  and then Token_Buffer.Token.Kind = String_Literal;
   --  Append the given codepoint to Token_Buffer.Token.String_Value (a UTF-8
   --  encoded string).

   function Digit_Value
     (Codepoint : Wide_Wide_Character) return Interfaces.Unsigned_64
      with Pre => Codepoint in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';
   --  Assuming that Codepoint is a valid hexadecimal digit, return the
   --  corresponding number value.

   function Read_Unicode_Escape_Sequence
     (Location : Source_Location) return Boolean;
   --  Helper for Read_Quoted_String. Assuming that string parsing just read a
   --  "\u" or "\U" prefix, parse the rest of the Unicode escape sequence and
   --  append the denoted codepoint using Append_As_UTF8. If successful, return
   --  True, otherwise put an error in Result and return False.

   function Read_Quoted_String return Boolean;
   --  Helper for Read_Token. Read a string literal, whose first quote is in
   --  Codepoint_Buffer. Return whether successful, updating Token_Buffer
   --  accordingly.

   function Read_Number_Like return Boolean;
   --  Helper for Read_Token. Read an integer literal or a local date, whose
   --  first digit (or sign) is in Codepoint_Buffer. Return whether successful,
   --  updating Token_Buffer accordingly.

   function Read_Datetime_Field
     (What        : String;
      Digit_Count : Positive;
      Base_Value  : Interfaces.Unsigned_64;
      Base_Digits : Natural;
      Value       : out Interfaces.Unsigned_64) return Boolean;
   --  Helper to read dates and times. Read a specific date/time field: What is
   --  the name of the field to read. Considering that we already read
   --  Base_Digits number of digits and that the decoded value so far is
   --  Base_Value, continue consuming digits until we have a total of
   --  Digit_Count digits. Put the decoded value in Value and return True if
   --  successful. Create a lexing error and return False otherwise.

   function Read_Date
     (Base_Value : Interfaces.Unsigned_64; Base_Digits : Natural)
      return Boolean;
   --  Helper for Read_Number_Like. Read a local date, local datetime or an
   --  offset datetime, considering that we already consumed Base_Digits
   --  digits, whose value is Base_Value. Return whether successful, updating
   --  Token_Buffer accordingly.

   function Read_Local_Time
     (Base_Value    : Interfaces.Unsigned_64;
      Base_Digits   : Natural;
      Read_Timezone : Boolean) return Boolean;
   --  Helper for Read_Number_Like and Read_Date. Read a local time,
   --  considering that we already consumed Base_Digits digits, whose value
   --  is Base_Value. Return whether successful, updating Token_Buffer
   --  accordingly (Local_Time_Literal).
   --
   --  If Read_Timezone is true, attempt to read the timezone information. In
   --  this mode, assume that Token_Buffer already contains a date
   --  (Local_Date_Literal) and update it to contain either an
   --  Offset_Datetime_Literal variant (if there is a timezone) or a
   --  Local_Datetime_Literal variant.

   function Read_Float
     (Positive      : Boolean;
      Integer_Value : Interfaces.Unsigned_64) return Boolean;
   --  Helper for Read_Number_Like. Read a floating point number considering
   --  that we already consumed its sign (Positive) and its integer part
   --  (Integer_Part): the codepoint buffer must contain one of '.' | 'e' |
   --  'E'. Return whether successful, updating Token_Buffer accordingly.

   function Read_Special_Float
     (Name     : Wide_Wide_String;
      Kind     : Special_Float_Kind;
      Positive : Boolean) return Boolean;
   --  Helper for Read_Number_Like. Read the name of a special floating point
   --  number. Return whether successful, updating Token_Buffer accordingly.

   function Read_Bare_Key return Boolean;
   --  Helper for Read_Token. Read a bare key, whose first character is in
   --  Codepoint_Buffer. Return whether successful, updating Token_Buffer
   --  accordingly.

   function Read_Keyword
     (Text : Wide_Wide_String; Token : Any_Token) return Boolean;
   --  Helper for Read_Token. Try to read the given Text, whose first character
   --  is in Codepoint_Buffer. Return whether successful, updating Token_Buffer
   --  accordingly.

   --------------------
   -- Syntax parsing --
   --------------------

   function Parse_Line return Boolean;
   --  Read and parse the next line in Stream. Update Result accordingly.

   ------------------------
   -- Table construction --
   ------------------------

   function Get_Table
     (Key             : Unbounded_UTF8_String;
      Table           : in out TOML_Value;
      Traverse_Arrays : Boolean := False) return Boolean;
   --  Look for a sub-table in Table correspondig to Key. On success, store it
   --  in Table and return True. Otherwise, return False and put an error in
   --  Result.
   --
   --  If there is no Key entry in Table, create one and register it.
   --
   --  If this entry is an array, then assuming all the following hold:
   --
   --  * Traverse_Arrays is true;
   --  * Table contains a Key entry that is an array of tables;
   --  * this array contains at least one element.
   --
   --  then this gets the last table that was added to this array. Otherwise,
   --  this fails.

   function Parse_Section (Array_Of_Table : Boolean) return Boolean;
   --  Parse the name of a section ([my.table]) and update Current_Table
   --  accordingly. This assumes that the opening bracket is already in
   --  Token_Buffer. When this returns, the Newline token (or EOF) is in
   --  Token_Buffer.
   --
   --  If Array_Of_Table is false, expect the section name to designate a
   --  table. Otherwise, expect it to designate an array of tables.
   --
   --  Return whether parsing and interpretation was successful.

   function Parse_Dotted_Keys
     (Table           : in out TOML_Value;
      Key             : out Unbounded_UTF8_String;
      Traverse_Arrays : Boolean := False) return Boolean;
   --  Parse a sequence of dotted keys and interpret it as a reference into
   --  Table. Put the last key in Key, and update Table to the referenced table
   --  (i.e. the one in which Key will reference/create an entry).
   --
   --  This assumes that the first token that constitutes the dotted keys is in
   --  Token_Buffer. When this returns, the token just passed the dotted keys
   --  will be in Token_Buffer.
   --
   --  Return whether parsing and interpretation was successful.

   function Parse_Value (Value : out TOML_Value) return Boolean;
   --  Parse an inline value (boolean/integer literal, inline table, inline
   --  array, ...) and put it in Value.
   --
   --  The first token that constitutes the value is expected to be read during
   --  the call. When this returns, Token_Buffer contains the last token that
   --  was used to parse the value.

   function Parse_Array (Value : out TOML_Value) return Boolean;
   --  Parse an inline array and put it in Value.
   --
   --  This assumes that the first token that constitutes the array (i.e. '[')
   --  is in Token_Buffer. When this returns, Token_Buffer contains the last
   --  token that was used to parse the value (i.e. ']').

   function Parse_Table (Value : out TOML_Value) return Boolean;
   --  Parse an inline table and put it in Value.
   --
   --  This assumes that the first token that constitutes the table (i.e. '{')
   --  is in Token_Buffer. When this returns, Token_Buffer contains the last
   --  token that was used to parse the value (i.e. '}').

   ------------------
   -- Create_Error --
   ------------------

   function Create_Error
     (Message  : String;
      Location : Source_Location := No_Location) return Boolean is
   begin
      Result :=
        (Success  => False,
         Message  => To_Unbounded_String (Message),
         Location => (if Location = No_Location
                      then Codepoint_Buffer.Location
                      else Location));
      return False;
   end Create_Error;

   -------------------------
   -- Create_Lexing_Error --
   -------------------------

   function Create_Lexing_Error
     (Message : String := "invalid token") return Boolean is
   begin
      return Create_Error (Message, Token_Buffer.Location);
   end Create_Lexing_Error;

   -------------------------
   -- Create_Syntax_Error --
   -------------------------

   function Create_Syntax_Error
     (Message : String := "invalid syntax") return Boolean is
   begin
      return Create_Error (Message, Token_Buffer.Location);
   end Create_Syntax_Error;

   --------------------
   -- Read_Codepoint --
   --------------------

   function Read_Codepoint return Boolean is
      use type Interfaces.Unsigned_32;

      Error_Message : constant String := "invalid UTF-8 encoding";

      EOF  : Boolean;
      Char : Character;
      --  Holders for Get OUT formals

      Bytes_Count : Positive;
      --  Number of bytes that encode the codepoint to read

      Min_Codepoint : constant array (1 .. 4) of Interfaces.Unsigned_32 :=
        (1 => 16#00#,
         2 => 16#80#,
         3 => 16#80_00#,
         4 => 16#1_00_00#);
      --  For each Bytes_Count, smallest codepoint that is allowed

      Byte : Interfaces.Unsigned_32;
      --  Numeric view for Char

      Result : Interfaces.Unsigned_32 := 0;
      --  Numeric view for the Result

   begin
      --  If we are supposed to re-emit the previously read codepoint, do
      --  nothing more.

      if Codepoint_Buffer.To_Reemit then
         Codepoint_Buffer.To_Reemit := False;
         return True;
      end if;

      --  Try to read the first byte

      Get (Stream, EOF, Char);
      if EOF then
         Codepoint_Buffer :=
           (EOF       => True,
            To_Reemit => False,
            Location  => (Codepoint_Buffer.Location.Line,
                          Codepoint_Buffer.Location.Column + 1));
         return True;
      else
         Byte := Character'Pos (Char);
      end if;

      --  ASCII characters are always one byte long, so we can handle them
      --  right now: process ASCII's control characters.

      case Char is

      when ASCII.NUL .. ASCII.BS
         | ASCII.VT
         | ASCII.FF
         | ASCII.SO .. ASCII.US
         | ASCII.DEL =>

         return Create_Error ("invalid ASCII control character");

      when ASCII.CR =>

         --  If this is a carriage return, discarding after checking that it is
         --  followed by a linefeed.

         Get (Stream, EOF, Char);
         if EOF or else Char /= ASCII.LF then
            return Create_Error ("invalid stray carriage return");
         end if;

         Byte := Character'Pos (Char);

      when others =>
         null;
      end case;

      --  Special handling of source location update occurs only with pure
      --  ASCII characters, so we can handle it here, too.

      if Codepoint_Buffer.Location = No_Location then
         Codepoint_Buffer.Location := (1, 1);
      else
         case Char is
            when ASCII.LF =>
               Codepoint_Buffer.Location :=
                 (Codepoint_Buffer.Location.Line + 1, 1);
            when ASCII.HT =>
               declare
                  Column     : Natural := Codepoint_Buffer.Location.Column;
                  Column_Mod : constant Natural := Column mod Tab_Stop;
               begin
                  if Column_Mod = 0 then
                     Column := Column + Tab_Stop;
                  else
                     Column := Column + Tab_Stop - Column_Mod;
                  end if;
                  Codepoint_Buffer.Location.Column := Column;
               end;

            when others =>
               Codepoint_Buffer.Location.Column :=
                  Codepoint_Buffer.Location.Column + 1;
         end case;
      end if;

      --  Its leading bits tell us how many bytes are to be expected

      if (Byte and 2#1000_0000#) = 0 then
         Bytes_Count := 1;
         Result := Byte and 2#0111_1111#;

      elsif (Byte and 2#1110_0000#) = 2#1100_0000# then
         Bytes_Count := 2;
         Result := Byte and 2#0001_1111#;

      elsif (Byte and 2#1111_0000#) = 2#1110_0000# then
         Bytes_Count := 3;
         Result := Byte and 2#0000_1111#;

      elsif (Byte and 2#1111_1000#) = 2#1111_0000# then
         Bytes_Count := 4;
         Result := Byte and 2#0000_0111#;

      else
         return Create_Error (Error_Message);
      end if;

      --  Read the remaining bytes. We know how many we must read, so if we
      --  reach EOF in the process, we know it's an error.

      for I in 2 .. Bytes_Count loop
         Get (Stream, EOF, Char);
         Byte := Character'Pos (Char);

         if EOF or else (Byte and 2#1100_0000#) /= 2#1000_0000# then
            return Create_Error (Error_Message);
         end if;

         Result := 64 * Result + (Byte and 2#0011_1111#);
      end loop;

      --  Check that the codepoint is as big as the number of bytes allows

      if Result < Min_Codepoint (Bytes_Count) then
         return Create_Error (Error_Message);
      end if;

      Codepoint_Buffer.Codepoint := Wide_Wide_Character'Val (Result);

      --  Reject surrogate codepoints

      if Codepoint_Buffer.Codepoint in WW_Surrogates then
         return Create_Error ("surrogate codepoints are invalid");
      end if;

      return True;
   end Read_Codepoint;

   ----------------------
   -- Reemit_Codepoint --
   ----------------------

   procedure Reemit_Codepoint is
   begin
      Codepoint_Buffer.To_Reemit := True;
   end Reemit_Codepoint;

   ----------------
   -- Read_Token --
   ----------------

   function Read_Token (Key_Expected : Boolean) return Boolean is
   begin
      --  If we are supposed to re-emit the previously read token, do nothing
      --  more.

      if Token_Buffer.To_Reemit then
         Token_Buffer.To_Reemit := False;
         return True;
      end if;

      loop
         --  The location for this token is the location of the codepoint
         --  cursor before reading it. Special case for the first token:
         --  codepoint location is not initialized yet, so use the actual first
         --  location.

         Token_Buffer.Location := Codepoint_Buffer.Location;
         if Token_Buffer.Location = No_Location then
            Token_Buffer.Location := (1, 1);
         end if;

         --  Try to read the first codepoint. If we reached the end of file,
         --  communicate this fact to the caller and stop there.

         if not Read_Codepoint then
            return False;
         end if;

         if Codepoint_Buffer.EOF then
            Token_Buffer := (EOF => True, To_Reemit => False);
            return True;
         end if;

         --  Otherwise, see what we can do with this codepoint

         case Codepoint_Buffer.Codepoint is

            --  If this is a newline (either LF or CR-LF), return the
            --  corresponding token.

            when WW_Linefeed =>
               Token_Buffer.Location := Codepoint_Buffer.Location;
               Token_Buffer.Token := (Kind => Newline);
               return True;

            when WW_Carriage_Return =>
               if not Read_Codepoint then
                  return False;
               elsif Codepoint_Buffer.EOF then
                  return Create_Lexing_Error ("trailing CR");
               elsif Codepoint_Buffer.Codepoint /= WW_Linefeed then
                  return Create_Lexing_Error ("stray CR");
               end if;
               Token_Buffer.Token := (Kind => Newline);
               return True;

            --  Skip whitespaces

            when WW_Tab | ' ' =>
               null;

            --  Skip comments. Note that the end of file is allowed to occur in
            --  a comment, i.e. without a newline at the end.

            when '#' =>
               Comment_Loop : loop
                  if not Read_Codepoint then
                     return False;
                  elsif Codepoint_Buffer.EOF then
                     Token_Buffer := (EOF => True, To_Reemit => False);
                     return True;
                  elsif Codepoint_Buffer.Codepoint = WW_Linefeed then
                     --  Don't forget to re-emit the linefeed: it is not part
                     --  of the comment itself

                     Reemit_Codepoint;
                     exit Comment_Loop;
                  end if;
               end loop Comment_Loop;

            --  Parse no-text tokens that cannot be keys

            when '=' =>
               Token_Buffer.Token := (Kind => Equal);
               return True;

            when '.' =>
               Token_Buffer.Token := (Kind => Dot);
               return True;

            when ',' =>
               Token_Buffer.Token := (Kind => Comma);
               return True;

            when '{' =>
               Token_Buffer.Token := (Kind => Curly_Bracket_Open);
               return True;

            when '}' =>
               Token_Buffer.Token := (Kind => Curly_Bracket_Close);
               return True;

            when '[' =>
               if not Read_Codepoint then
                  return False;
               elsif Key_Expected
                     and then not Codepoint_Buffer.EOF
                     and then Codepoint_Buffer.Codepoint = '['
               then
                  Token_Buffer.Token := (Kind => Double_Square_Bracket_Open);
               else
                  Reemit_Codepoint;
                  Token_Buffer.Token := (Kind => Square_Bracket_Open);
               end if;
               return True;

            when ']' =>
               if not Read_Codepoint then
                  return False;
               elsif Key_Expected
                     and then not Codepoint_Buffer.EOF
                     and then Codepoint_Buffer.Codepoint = ']'
               then
                  Token_Buffer.Token := (Kind => Double_Square_Bracket_Close);
               else
                  Reemit_Codepoint;
                  Token_Buffer.Token := (Kind => Square_Bracket_Close);
               end if;
               return True;

            --  Parse string literals

            when '"' | ''' =>
               return Read_Quoted_String;

            --  The rest is potential bare keys (bare keys, integer literals,
            --  boolean literals, ...).

            when '0' .. '9' =>
               if Key_Expected then
                  return Read_Bare_Key;
               else
                  return Read_Number_Like;
               end if;

            when 'A' .. 'Z' | 'a' .. 'z' | '_' =>
               if Key_Expected then
                  return Read_Bare_Key;
               end if;

               --  Detect keywords: "false" or "true". TODO: handle "inf" and
               --  "nan".

               case Codepoint_Buffer.Codepoint is
                  when 'f' =>
                     return Read_Keyword ("false", False_Keyword);
                  when 't' =>
                     return Read_Keyword ("true", True_Keyword);
                  when 'n' =>
                     return Read_Special_Float ("nan", NaN, True);
                  when 'i' =>
                     return Read_Special_Float ("inf", Infinity, True);
                  when others =>
                     return Create_Lexing_Error;
               end case;

            when '+' =>
               return Read_Number_Like;

            when '-' =>
               if Key_Expected then
                  return Read_Bare_Key;
               else
                  return Read_Number_Like;
               end if;

            when others =>
               return Create_Lexing_Error;
         end case;
      end loop;
   end Read_Token;

   ------------------
   -- Reemit_Token --
   ------------------

   procedure Reemit_Token is
   begin
      Token_Buffer.To_Reemit := True;
   end Reemit_Token;

   --------------------
   -- Append_As_UTF8 --
   --------------------

   procedure Append_As_UTF8 (Codepoint : Wide_Wide_Character) is
      use Interfaces;

      Ordinal : constant Unsigned_32 := Wide_Wide_Character'Pos (Codepoint);

      function Bit_Slice (LSB, Width : Natural) return Unsigned_8;
      --  Return the slice of bits in Codepoint/Ordinal starting at the given
      --  0-based index Least Significant Bit (LSB) and that contains Width
      --  bits.

      function Bit_Slice (LSB, Width : Natural) return Unsigned_8 is
         Shifted : constant Unsigned_32 := Shift_Right (Ordinal, LSB);
         Mask    : constant Unsigned_32 := 2 ** Width - 1;
      begin
         return Unsigned_8 (Shifted and Mask);
      end Bit_Slice;

      Bytes     : array (1 .. 4) of Unsigned_8;
      Last_Byte : Positive;
   begin
      case Ordinal is
         when 16#0000# .. 16#007F# =>
            Bytes (1) := Bit_Slice (0, 7);
            Last_Byte := 1;
         when 16#0080# .. 16#07FF# =>
            Bytes (1) := 2#1100_0000# or Bit_Slice (6, 5);
            Bytes (2) := 2#1000_0000# or Bit_Slice (0, 6);
            Last_Byte := 2;
         when 16#0800# .. 16#FFFF# =>
            Bytes (1) := 2#1110_0000# or Bit_Slice (12, 4);
            Bytes (2) := 2#1000_0000# or Bit_Slice (6, 6);
            Bytes (3) := 2#1000_0000# or Bit_Slice (0, 6);
            Last_Byte := 3;
         when 16#1_0000# .. 16#10_FFFF# =>
            Bytes (1) := 2#1111_0000# or Bit_Slice (18, 3);
            Bytes (2) := 2#1000_0000# or Bit_Slice (12, 6);
            Bytes (3) := 2#1000_0000# or Bit_Slice (6, 6);
            Bytes (4) := 2#1000_0000# or Bit_Slice (0, 6);
            Last_Byte := 4;
         when 16#11_0000# .. Unsigned_32'Last =>
            --  Given that Codepoint comes from decoded UTF-8, this alternative
            --  should be unreachable.
            raise Program_Error;
      end case;

      declare
         Str : String (1 .. Last_Byte) with Import, Address => Bytes'Address;
      begin
         Append (Token_Buffer.Token.String_Value, Str);
      end;
   end Append_As_UTF8;

   -----------------
   -- Digit_Value --
   -----------------

   function Digit_Value
     (Codepoint : Wide_Wide_Character) return Interfaces.Unsigned_64
   is
      use Interfaces;
      CP : constant Unsigned_64 :=
         Wide_Wide_Character'Pos (Codepoint_Buffer.Codepoint);
   begin
      case Codepoint is
         when '0' .. '9' =>
            return CP - Wide_Wide_Character'Pos ('0');
         when 'a' .. 'f' =>
            return CP - Wide_Wide_Character'Pos ('a') + 10;
         when 'A' .. 'F' =>
            return CP - Wide_Wide_Character'Pos ('A') + 10;
         when others =>
            raise Program_Error;
      end case;
   end Digit_Value;

   ----------------------------------
   -- Read_Unicode_Escape_Sequence --
   ----------------------------------

   function Read_Unicode_Escape_Sequence
     (Location : Source_Location) return Boolean
   is
      use Interfaces;

      Size : constant Positive :=
        (if Codepoint_Buffer.Codepoint = 'u' then 4 else 8);
      --  Number of digits to read

      CP : Unsigned_32;
      --  Temporary buffer for the codepoint we just read

      Result : Unsigned_32 := 0;
      --  Denoted Unicode codepoint
   begin
      for I in 1 .. Size loop
         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF then
            return Create_Error
              ("unterminated Unicode escape sequence", Location);
         end if;

         --  Turn the hexadecimal digit we just read into a number and add it
         --  to Result, or raise an error if this is not a valid digit.

         CP := Wide_Wide_Character'Pos (Codepoint_Buffer.Codepoint);
         declare
            Denoted_Digit : Unsigned_32;
         begin
            if Codepoint_Buffer.Codepoint in '0' .. '9' then
               Denoted_Digit := CP - Wide_Wide_Character'Pos ('0');
            elsif Codepoint_Buffer.Codepoint in 'a' .. 'f' then
               Denoted_Digit := CP - Wide_Wide_Character'Pos ('a') + 10;
            elsif Codepoint_Buffer.Codepoint in 'A' .. 'F' then
               Denoted_Digit := CP - Wide_Wide_Character'Pos ('A') + 10;
            else
               return Create_Error
                 ("invalid Unicode escape sequence", Location);
            end if;
            Result := 16 * Result + Denoted_Digit;
         end;
      end loop;

      declare
         Codepoint : constant Wide_Wide_Character :=
            Wide_Wide_Character'Val (Result);
      begin
         --  Reject surrogate codepoints

         if Codepoint in WW_Surrogates then
            return Create_Error ("surrogate codepoints are invalid", Location);
         end if;

         Append_As_UTF8 (Codepoint);
         return True;
      end;
   end Read_Unicode_Escape_Sequence;

   ------------------------
   -- Read_Quoted_String --
   ------------------------

   function Read_Quoted_String return Boolean is
      Delimiter : constant Wide_Wide_Character :=
         Codepoint_Buffer.Codepoint;

      Is_Literal   : constant Boolean := Delimiter = ''';
      Is_Multiline : Boolean := False;

      function Unterminated_String return Boolean is
        (Create_Error ("unterminated string"));

      function Skip_Line_Ending_Backslash return Boolean;
      --  Assuming that, in a multiline string, we just processed a backslash
      --  followed by a whitespace or a line feed (i.e. a line ending
      --  backslash), skip all the following whitespaces/line feeds/backslashes
      --  that should be ignored when processing the string. Return whether
      --  successful (i.e. if there is no syntax error).

      Location : Source_Location;

      --------------------------------
      -- Skip_Line_Ending_Backslash --
      --------------------------------

      function Skip_Line_Ending_Backslash return Boolean is
         Line_Ended : Boolean := Codepoint_Buffer.Codepoint = WW_Linefeed;
         --  Whether we processed a line feed after the last backslash. We keep
         --  track of this to make sure that only whitespaces can follow line
         --  ending backslashes.
      begin
         loop
            if not Read_Codepoint then
               return False;
            elsif Codepoint_Buffer.EOF then
               return Unterminated_String;
            elsif Codepoint_Buffer.Codepoint not in ' ' | WW_Tab | WW_Linefeed
            then
               --  We found something that must not be
               --  discarded: re-emit it so that the next
               --  loop iteration processes it.

               Reemit_Codepoint;
               exit;

            elsif Line_Ended and then Codepoint_Buffer.Codepoint = '\' then

               --  On a separate line, we found a new line ending backslash:
               --  skip it and only expect whitespaces before the next new
               --  line.

               Line_Ended := False;

            else
               Line_Ended :=
                  Line_Ended or else Codepoint_Buffer.Codepoint in WW_Linefeed;
            end if;
         end loop;

         --  There must be a line feed before the first non-whitespace
         --  codepoint that follows a line ending backslash.

         return (Line_Ended
                 or else Create_Error ("invalid escape sequence", Location));
      end Skip_Line_Ending_Backslash;
   begin
      --  Read the potential first character of this string.

      if not Read_Codepoint then
         return False;
      elsif Codepoint_Buffer.EOF then
         return Create_Error ("unterminated string");
      end if;

      Token_Buffer.Token :=
        (Kind          => String_Literal,
         String_Format =>
           (if Is_Literal then Literal_String else Basic_String),
         others        => <>);

      --  If we have a second delimiter, then check if this is a multi-line
      --  string.

      if Codepoint_Buffer.Codepoint = Delimiter then

         if not Read_Codepoint then
            return False;

         elsif Codepoint_Buffer.EOF
               or else Codepoint_Buffer.Codepoint /= Delimiter
         then
            --  We found two delimiters not followed by a third one: this
            --  designates an empty string.

            Reemit_Codepoint;
            return True;

         else
            --  We have three consecutive delimiters: this is a multiline
            --  string.

            Is_Multiline := True;
            Token_Buffer.Token.String_Format :=
              (if Is_Literal
               then Multiline_Literal_String
               else Multiline_Basic_String);
         end if;

      else
         --  Never mind: no second delimiter, so put this codepoint back to be
         --  read again in the scanning loop.

         Reemit_Codepoint;
      end if;

      --  Now, go through all codepoints until we find the closing
      --  delimiter(s).

      loop
         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF then
            return Create_Error ("unterminated string");
         end if;

         Location := Codepoint_Buffer.Location;
         case Codepoint_Buffer.Codepoint is
            when '"' | ''' =>
               if Codepoint_Buffer.Codepoint /= Delimiter then
                  --  If this character is not the string delimiter, include it
                  --  as-is in the denoted string.

                  Append_As_UTF8 (Codepoint_Buffer.Codepoint);

               elsif not Is_Multiline then
                  --  Otherwise, if the string is not multi-line, it's the end
                  --  of our string.

                  return True;

               --  At this point, we know this string is multiline: look for
               --  the two next characters: if they are all delimiters, this
               --  is the end of our string. Otherwise, continue reading
               --  them...

               elsif not Read_Codepoint then
                  return False;

               elsif Codepoint_Buffer.EOF then
                  return Unterminated_String;

               elsif Codepoint_Buffer.Codepoint /= Delimiter then
                  --  The second delimiter is missing: append the first one
                  --  and schedule to re-examine the current codepoint in
                  --  another loop iteration.

                  Append_As_UTF8 (Delimiter);
                  Reemit_Codepoint;

               --  Read the third character...

               elsif not Read_Codepoint then
                  return False;

               elsif Codepoint_Buffer.EOF then
                  return Unterminated_String;

               elsif Codepoint_Buffer.Codepoint /= Delimiter then
                  --  The third delimiter is missing: append the first two ones
                  --  and schedule to re-examine the current codepoint in
                  --  another loop iteration.

                  Append_As_UTF8 (Delimiter);
                  Append_As_UTF8 (Delimiter);
                  Reemit_Codepoint;

               --  We just got the third delimiter, but we can still have (a)
               --  one or (b) two more delimiters: in that case the first (a)
               --  one or (b) two are part of the string, and the last three
               --  are closing the string. So try reading a fourth delimiter...

               elsif not Read_Codepoint then
                  return False;

               elsif Codepoint_Buffer.EOF
                     or else Codepoint_Buffer.Codepoint /= Delimiter
               then
                  --  No fourth delimiter: put back the EOF/codepoint and
                  --  return a successful multiline string parsing.

                  Reemit_Codepoint;
                  return True;

               else
                  --  We got a fourth delimiter: append one to the denoted
                  --  string and try to read the fifth one...

                  Append_As_UTF8 (Delimiter);

                  if not Read_Codepoint then
                     return False;
                  elsif Codepoint_Buffer.EOF
                        or else Codepoint_Buffer.Codepoint /= Delimiter
                  then
                     --  Just like above, if we could not get one, just put the
                     --  codepoint back and return success.

                     Reemit_Codepoint;

                  else
                     --  We got a fifth delimiter: append yet another delimiter
                     --  (the second one) to the denoted string and consider
                     --  the read done.

                     Append_As_UTF8 (Delimiter);
                  end if;
                  return True;
               end if;

            when '\' =>
               --  If this is a literal string, we have a literal backslash.

               if Is_Literal then
                  Append_As_UTF8 (Codepoint_Buffer.Codepoint);

               --  This starts an escape sequence: read the next character to
               --  find out which.

               elsif not Read_Codepoint then
                  return False;
               elsif Codepoint_Buffer.EOF then
                  return Unterminated_String;
               else
                  case Codepoint_Buffer.Codepoint is
                     when 'b' => Append_As_UTF8 (WW_Backspace);
                     when 't' => Append_As_UTF8 (WW_Tab);
                     when 'n' => Append_As_UTF8 (WW_Linefeed);
                     when 'f' => Append_As_UTF8 (WW_Form_Feed);
                     when 'r' => Append_As_UTF8 (WW_Carriage_Return);
                     when '"' => Append_As_UTF8 ('"');
                     when '\' => Append_As_UTF8 ('\');

                     when 'u' | 'U' =>
                        if not Read_Unicode_Escape_Sequence (Location) then
                           return False;
                        end if;

                     when ' ' | WW_Tab | WW_Linefeed =>

                        --  This is valid only for multi-line strings. If
                        --  literal, just append these codepoints. Otherwise we
                        --  we just found a "line ending backslash": discard
                        --  all the whitespace characters we find next.

                        if not Is_Multiline then
                           return Unterminated_String;
                        elsif Is_Literal then
                           Append_As_UTF8 (WW_Linefeed);
                        elsif not Skip_Line_Ending_Backslash then
                           return False;
                        end if;

                     when others =>
                        return Create_Error
                          ("invalid escape sequence", Location);
                  end case;
               end if;

            when WW_Control_Characters =>
               --  Linefeeds are allowed only for multi-line strings.

               if Codepoint_Buffer.Codepoint = WW_Linefeed
                  and then Is_Multiline
               then
                  --  When they are the first codepoint after the string
                  --  delimiter, they are discarded.

                  if Length (Token_Buffer.Token.String_Value) > 0 then
                     Append_As_UTF8 (WW_Linefeed);
                  end if;

               elsif Codepoint_Buffer.Codepoint = WW_Tab then

                  --  Horizontal tabs are always allowed

                  Append_As_UTF8 (WW_Tab);

               else
                  return Create_Error ("invalid string", Location);
               end if;

            when others =>
               Append_As_UTF8 (Codepoint_Buffer.Codepoint);
         end case;
      end loop;
   end Read_Quoted_String;

   ----------------------
   -- Read_Number_Like --
   ----------------------

   function Read_Number_Like return Boolean is
      use Interfaces;

      type Any_Format is (Decimal, Hexadecimal, Binary, Octal);
      Format : Any_Format := Decimal;
      --  Format for the integer to parse

      Base : constant array (Any_Format) of Unsigned_64 :=
        (Decimal => 10, Hexadecimal => 16, Binary => 2, Octal => 8);

      type Any_Sign is (None, Positive, Negative);
      Sign          : Any_Sign := None;
      Sign_Explicit : Boolean := False;
      --  Sign for the integer to parse, and whether it was explicit

      Abs_Value : Interfaces.Unsigned_64 := 0;
      --  Absolute value for the integer that is parsed

      Digit_Count : Natural := 0;
      --  Number of digit codepoints consumed

      Had_Underscore : Boolean := False;
      --  Whether we found an underscore at all for this token

      Leading_Zero : Boolean := False;
      --  Whether this number starts with a zero

      Just_Passed_Underscore : Boolean := False;
      --  Whether the last codepoint processed was an underscore

      function Reject_Leading_Zero return Boolean;
      --  If Leading_Zero is true, create an error and return False. Return
      --  true otherwise.

      function Reject_Passed_Underscore return Boolean;
      --  If Just_Passed_Underscore is true, create an error and return False.
      --  Return true otherwise.

      -------------------------
      -- Reject_Leading_Zero --
      -------------------------

      function Reject_Leading_Zero return Boolean is
      begin
         if Leading_Zero then
            return Create_Lexing_Error
              ("leading zeros are not allowed in decimals");
         end if;
         return True;
      end Reject_Leading_Zero;

      ------------------------------
      -- Reject_Passed_Underscore --
      ------------------------------

      function Reject_Passed_Underscore return Boolean is
      begin
         if Just_Passed_Underscore then
            return Create_Lexing_Error
              ("underscores must be surrounded by digits");
         end if;
         return True;
      end Reject_Passed_Underscore;

      function Too_Large_Error return Boolean is
        (Create_Error ("too large integer", Token_Buffer.Location));
   begin
      Token_Buffer.Token := (Kind => Integer_Literal, Integer_Value => 0);

      --  Decode the sign, if any

      if Codepoint_Buffer.Codepoint in '+' | '-' then
         Sign := (if Codepoint_Buffer.Codepoint = '+'
                  then Positive
                  else Negative);
         Sign_Explicit := True;
         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF then
            return Create_Lexing_Error;
         end if;
      end if;

      --  If this token starts with 0, we have either:
      --
      --  * just 0, i.e. the next character cannot be a digit as leading zeros
      --    are forbidden.
      --
      --  * 'b' (for binary literals), 'x' (for hexadecimal) or 'o' (for
      --    octal).

      if Codepoint_Buffer.Codepoint = '0' then
         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF then
            Reemit_Codepoint;
            return True;
         end if;

         case Codepoint_Buffer.Codepoint is
            when '_' | '0' .. '9' =>
               Digit_Count := Digit_Count + 1;
               Leading_Zero := True;

            when 'b' =>
               Format := Binary;
            when 'o' =>
               Format := Octal;
            when 'x' =>
               Format := Hexadecimal;

            when '.' | 'e' | 'E' =>
               return Read_Float (Sign /= Negative, Abs_Value);

            when others =>
               --  Consider all other codepoints as token separators

               Reemit_Codepoint;
               return True;
         end case;

         --  Explicit signs are valid only for numbers in decimal form

         if Format /= Decimal and then Sign_Explicit then
            return Create_Lexing_Error
              ("explicit sign not allowed but for decimals");
         end if;

         --  If we had a format specifier sequence, read the next codepoint,
         --  which will be the first digit of the number to read.

         if Format /= Decimal then
            if not Read_Codepoint then
               return False;
            elsif Codepoint_Buffer.EOF then
               Reemit_Codepoint;
               return True;
            elsif Codepoint_Buffer.Codepoint = '_' then
               return Create_Lexing_Error
                 ("underscores must be surrounded by digits");
            end if;
         end if;

      --  Check for special float values ("nan" and "inf")

      elsif Codepoint_Buffer.Codepoint = 'n' then
         return Read_Special_Float ("nan", NaN, Sign /= Negative);
      elsif Codepoint_Buffer.Codepoint = 'i' then
         return Read_Special_Float ("inf", Infinity, Sign /= Negative);
      end if;

      --  Now read and decode all digits for this token

      loop
         declare
            Is_Digit : Boolean := False;
            Digit    : Unsigned_64;
         begin
            --  See if we have a digit

            case Codepoint_Buffer.Codepoint is
               when '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' =>
                  if Format = Decimal
                     and then Codepoint_Buffer.Codepoint in 'e' | 'E'
                  then
                     return Reject_Leading_Zero and then
                            Reject_Passed_Underscore and then
                            Read_Float (Sign /= Negative, Abs_Value);
                  end if;

                  Is_Digit := True;
                  Digit := Digit_Value (Codepoint_Buffer.Codepoint);

               when '_' =>
                  Had_Underscore := True;
                  if Just_Passed_Underscore then
                     return Reject_Passed_Underscore;
                  else
                     Just_Passed_Underscore := True;
                  end if;

               when 'g' .. 'z' | 'G' .. 'Z' | WW_Non_ASCII =>

                  --  These codepoints cannot start a new token and yet they
                  --  are invalid elements for integer literals: this is an
                  --  error.

                  return Create_Lexing_Error;

               when '-' | ':' =>

                  --  If we had no sign, no underscore and no base specifier,
                  --  we have a local date ('-') or local time (':').

                  if Sign /= None
                     or else Format /= Decimal
                     or else Had_Underscore
                  then
                     Reemit_Codepoint;
                     exit;
                  end if;

                  if Codepoint_Buffer.Codepoint = '-' then
                     return Read_Date (Abs_Value, Digit_Count);
                  else
                     return Read_Local_Time (Abs_Value, Digit_Count,
                                             Read_Timezone => False);
                  end if;

               when '.' =>

                  --  If we had no base specifier, we have a floating point
                  --  number.
                  --
                  --  Floating point numbers can only use decimal digits, and
                  --  there must be at least one digit before the dot.

                  if Format /= Decimal or Digit_Count = 0 then
                     return Create_Lexing_Error ("invalid float");
                  end if;
                  return Reject_Leading_Zero and then
                         Reject_Passed_Underscore and then
                         Read_Float (Sign /= Negative, Abs_Value);

               when others =>
                  --  If we end up here, either we found the beginning of a new
                  --  token, or a token separator: stop reading the integer
                  --  right here.

                  Reemit_Codepoint;
                  exit;
            end case;

            --  Decode the digit (if we found one)

            if Is_Digit then
               Digit_Count := Digit_Count + 1;
               if Digit >= Base (Format) then
                  return Create_Lexing_Error;
               end if;

               declare
                  Next_Value : Unsigned_64;
               begin
                  begin
                     Next_Value := Base (Format) * Abs_Value + Digit;
                  exception
                     when Constraint_Error =>
                        return Too_Large_Error;
                  end;
                  Abs_Value := Next_Value;
               end;
               Just_Passed_Underscore := False;
            end if;

            --  Read the next digit. Consider this integer token done if we
            --  reached the end of stream.

            if not Read_Codepoint then
               return False;
            elsif Codepoint_Buffer.EOF then
               Reemit_Codepoint;
               exit;
            end if;
         end;
      end loop;

      --  If we reach this point, we know that the token is an integer (it's
      --  not a date or something else).

      if not Reject_Leading_Zero or else not Reject_Passed_Underscore then
         return False;
      end if;

      --  Apply the sign, making sure that there is no overflow in the process

      declare
         Rel_Value : Integer_64;
      begin
         if Sign = Negative then
            if Abs_Value <= Unsigned_64 (Integer_64'Last) then
               Rel_Value := -Integer_64 (Abs_Value);
            elsif Abs_Value = Unsigned_64 (Integer_64'Last) + 1 then
               Rel_Value := Integer_64'First;
            else
               return Too_Large_Error;
            end if;

         else
            if Abs_Value <= Unsigned_64 (Integer_64'Last) then
               Rel_Value := Integer_64 (Abs_Value);
            else
               return Too_Large_Error;
            end if;
         end if;

         Token_Buffer.Token.Integer_Value := Any_Integer (Rel_Value);
         return True;
      end;
   end Read_Number_Like;

   -------------------------
   -- Read_Datetime_Field --
   -------------------------

   function Read_Datetime_Field
     (What        : String;
      Digit_Count : Positive;
      Base_Value  : Interfaces.Unsigned_64;
      Base_Digits : Natural;
      Value       : out Interfaces.Unsigned_64) return Boolean
   is
      use Interfaces;

      function Create_Error return Boolean is
        (Create_Lexing_Error ("invalid " & What));
   begin
      if Base_Digits > Digit_Count then
         return Create_Error;
      end if;

      Value := Base_value;

      for DC in Base_Digits + 1 .. Digit_Count loop

         --  Decode the current digit and add it to the decoded value

         if Codepoint_Buffer.Codepoint not in '0' .. '9' then
            return Create_Error;
         end if;

         Value := 10 * Value + Digit_Value (Codepoint_Buffer.Codepoint);

         --  Read the next digit. We accept EOF only if we were able to read
         --  as many digits as requested.

         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF then
            exit when DC = Digit_Count;
            return Create_Error;
         end if;
      end loop;

      --  This check is not necessary for correctness. It improves user
      --  diagnostics: if a digit follows the last expected digit, we know
      --  there is an error.

      if not Codepoint_Buffer.EOF
         and then Codepoint_Buffer.Codepoint in '0' .. '9'
      then
         return Create_Error;
      end if;

      --  If we came across a codepoint, re-emit it so that the lexing loop
      --  can call Read_Token once more.

      if Codepoint_Buffer.EOF then
         Reemit_Codepoint;
      end if;

      return True;
   end Read_Datetime_Field;

   ---------------
   -- Read_Date --
   ---------------

   function Read_Date
     (Base_Value : Interfaces.Unsigned_64; Base_Digits : Natural)
      return Boolean
   is
      use Interfaces;

      subtype Year_Range is Unsigned_64 range
         Unsigned_64 (Any_Year'First) .. Unsigned_64 (Any_Year'Last);
      subtype Month_Range is Unsigned_64 range
         Unsigned_64 (Any_Month'First) .. Unsigned_64 (Any_Month'Last);
      subtype Day_Range is Unsigned_64 range
         Unsigned_64 (Any_Day'First) .. Unsigned_64 (Any_Day'Last);

      Year, Month, Day : Unsigned_64 := Base_Value;
      Datetime         : Any_Local_Datetime;
   begin
      --  Finish reading the year and consume the following dash

      if not Read_Datetime_Field ("year", 4, Base_Value, Base_Digits, Year)
      then
         return False;
      elsif Codepoint_Buffer.EOF or else Codepoint_Buffer.Codepoint /= '-' then
         return Create_Lexing_Error ("invalid year");
      end if;

      --  Now read the month and consume the following dash

      if not Read_Codepoint then
         return False;
      elsif Codepoint_Buffer.EOF then
         return Create_Lexing_Error ("invalid month");
      elsif not Read_Datetime_Field ("month", 2, 0, 0, Month) then
         return False;
      elsif Codepoint_Buffer.EOF or else Codepoint_Buffer.Codepoint /= '-' then
         return Create_Lexing_Error ("invalid month");
      end if;

      --  Now read the day

      if not Read_Codepoint then
         return False;
      elsif Codepoint_Buffer.EOF then
         return Create_Lexing_Error ("invalid day");
      elsif not Read_Datetime_Field ("day", 2, 0, 0, Day) then
         return False;
      end if;

      --  Check that all fields are in range.
      --
      --  TODO: check that the date they represent is valid. This is not
      --  trivial as TOML can handle all years between 0 and 9999 while
      --  Ada.Calendar supports only 1901 through 2399.

      if Year not in Year_Range then
         return Create_Lexing_Error ("out of range year");
      elsif Month not in Month_Range then
         return Create_Lexing_Error ("out of range month");
      elsif Day not in Day_Range then
         return Create_Lexing_Error ("out of range day");
      end if;

      Datetime.Date := (Year  => Any_Year (Year),
                        Month => Any_Month (Month),
                        Day   => Any_Day (Day));
      Token_Buffer.Token := (Kind             => Local_Date_Literal,
                             Local_Date_Value => Datetime.Date);

      --  Now try to read a local time: if there is one, we can create a local
      --  datetime, otherwise it's just a local date.

      if not Codepoint_Buffer.EOF
         and then Codepoint_Buffer.Codepoint in 'T' | 't'
      then
         --  If the first codepoint after the date is 'T', then we know we have
         --  a local time ahead.

         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF then
            return Create_Lexing_Error ("truncated datetime");
         end if;

      elsif not Codepoint_Buffer.EOF and then Codepoint_Buffer.Codepoint = ' '
      then
         --  If the first codepoint after the date is ' ', then we know we have
         --  a local time ahead iff a digit follows this space. Otherwise, just
         --  consider it's the beginning of another token.

         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF
               or else Codepoint_Buffer.Codepoint not in '0' .. '9'
         then
            Reemit_Codepoint;
            return True;
         end if;

      else
         Reemit_Codepoint;
         return True;
      end if;

      --  At this point, we know there is a local time ahead, so try to read it

      return Read_Local_Time (0, 0, Read_Timezone => True);
   end Read_Date;

   ---------------------
   -- Read_Local_Time --
   ---------------------

   function Read_Local_Time
     (Base_Value    : Interfaces.Unsigned_64;
      Base_Digits   : Natural;
      Read_Timezone : Boolean) return Boolean
   is
      use Interfaces;

      subtype Hour_Range is Unsigned_64 range
         Unsigned_64 (Any_Hour'First) .. Unsigned_64 (Any_Hour'Last);
      subtype Minute_Range is Unsigned_64 range
         Unsigned_64 (Any_Minute'First) .. Unsigned_64 (Any_Minute'Last);
      subtype Second_Range is Unsigned_64 range
         Unsigned_64 (Any_Second'First) .. Unsigned_64 (Any_Second'Last);
      subtype Millisecond_Range is Unsigned_64 range
         Unsigned_64 (Any_Millisecond'First)
         .. Unsigned_64 (Any_Millisecond'Last);

      Hour, Minute, Second, Millisecond : Unsigned_64 := Base_Value;

      Time : Any_Local_Time;
   begin
      --  Finish reading the hour and consume the following colon

      if not Read_Datetime_Field ("hour", 2, Base_Value, Base_Digits, Hour)
      then
         return False;
      elsif Codepoint_Buffer.EOF or else Codepoint_Buffer.Codepoint /= ':' then
         return Create_Lexing_Error ("invalid hour");
      end if;

      --  Now read the minute and consume the following colon

      if not Read_Codepoint then
         return False;
      elsif Codepoint_Buffer.EOF then
         return Create_Lexing_Error ("invalid minute");
      elsif not Read_Datetime_Field ("minute", 2, 0, 0, Minute) then
         return False;
      elsif Codepoint_Buffer.EOF or else Codepoint_Buffer.Codepoint /= ':' then
         return Create_Lexing_Error ("invalid minute");
      end if;

      --  Now read the second

      if not Read_Codepoint then
         return False;
      elsif Codepoint_Buffer.EOF then
         return Create_Lexing_Error ("invalid second");
      elsif not Read_Datetime_Field ("second", 2, 0, 0, Second) then
         return False;
      end if;

      --  If present, read a dot, and then read the millisecond

      Millisecond := 0;
      if Codepoint_Buffer.EOF or else Codepoint_Buffer.Codepoint /= '.' then
         Reemit_Codepoint;
      elsif not Read_Codepoint then
         return False;
      elsif Codepoint_Buffer.EOF then
         return Create_Lexing_Error ("truncated millisecond");
      else
         --  Read at least 1 digit and at most 3 digits, then discard other
         --  digits.

         declare
            Digit_Count : Natural := 0;
         begin
            while not Codepoint_Buffer.EOF
                  and then Codepoint_Buffer.Codepoint in '0' .. '9'
            loop
               if Digit_Count < 3 then
                  Digit_Count := Digit_Count + 1;
                  Millisecond := (10 * Millisecond
                                  + Digit_Value (Codepoint_Buffer.Codepoint));
               end if;

               if not Read_Codepoint then
                  return False;
               end if;
            end loop;

            --  Artificially append zeros so that what we really get is the
            --  number of milliseconds (".1" should give 100 milliseconds, not
            --  just 1).

            while Digit_Count < 3 loop
               Digit_Count := Digit_Count + 1;
               Millisecond := 10 * Millisecond;
            end loop;

            Reemit_Codepoint;
         end;
      end if;

      --  Check that all fields are in range

      if Hour not in Hour_Range then
         return Create_Lexing_Error ("out of range hour");
      elsif Minute not in Minute_Range then
         return Create_Lexing_Error ("out of range minute");
      elsif Second not in Second_Range then
         return Create_Lexing_Error ("out of range second");
      end if;

      --  The millisecond is read with 3 digits, and that's exactly what the
      --  millisecond range allows so this should be always true.

      pragma Assert (Millisecond in Millisecond_Range);

      Time := (Hour        => Any_Hour (Hour),
               Minute      => Any_Minute (Minute),
               Second      => Any_Second (Second),
               Millisecond => Any_Millisecond (Millisecond));

      if not Read_Timezone then
         Token_Buffer.Token :=
           (Kind             => Local_Time_Literal,
            Local_Time_Value => Time);
         return True;
      end if;

      --  We were asked to read the timezone information: assume the token
      --  buffer contains a date and complete it with the time, and the
      --  timezone if present.

      declare
         Datetime : constant Any_Local_Datetime :=
           (Date => Token_Buffer.Token.Local_Date_Value,
            Time => Time);

         --  Temporaries to build the local offset value that is read

         Positive_Offset : Boolean;
         --  Whether the offset is positive (starts with '+' or 'Z'). Note that
         --  RFC 3339 states that a negative offset with zero minutes means
         --  "unknown local offset".

         Hour_Offset   : Unsigned_64 := 0;
         Minute_Offset : Unsigned_64 := 0;
         --  Individual temporaries for the amount of ours and of minutes for
         --  the local offset.
      begin
         Token_Buffer.Token :=
           (Kind                 => Local_Datetime_Literal,
            Local_Datetime_Value => Datetime);

         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF
            or else Codepoint_Buffer.Codepoint not in 'Z' | 'z' | '+' | '-'
         then
            Reemit_Codepoint;
            return True;
         end if;

         if Codepoint_Buffer.Codepoint in 'Z' | 'z' then
            Positive_Offset := True;
         else
            Positive_Offset := Codepoint_Buffer.Codepoint = '+';

            --  Consume the hour offset and consume the colon

            if not Read_Codepoint then
               return False;
            elsif Codepoint_Buffer.EOF then
               return Create_Lexing_Error ("truncated hour offset");
            elsif not Read_Datetime_Field ("hour offset", 2, 0, 0, Hour_Offset)
            then
               return False;
            elsif Codepoint_Buffer.EOF
                  or else Codepoint_Buffer.Codepoint /= ':'
            then
               return Create_Lexing_Error ("invalid hour offset");
            end if;

            --  Now consume the minute offset

            if not Read_Codepoint then
               return False;
            elsif Codepoint_Buffer.EOF then
               return Create_Lexing_Error ("truncated minute offset");
            elsif not Read_Datetime_Field
              ("minute offset", 2, 0, 0, Minute_Offset)
            then
               return False;
            end if;

            --  Reemit the last codepoint as Read_Datetime_Field consumes the
            --  one that appears right after the last codepoint that is part of
            --  the datetime field.

            Reemit_Codepoint;
         end if;

         --  Check ranges for hours/minutes

         if Hour_Offset not in Hour_Range then
            return Create_Lexing_Error ("out of range hour offset");
         elsif Minute_Offset not in Minute_Range then
            return Create_Lexing_Error ("out of range minute offset");
         end if;

         --  Gather all decoded data to format the result

         declare
            Absolute_Offset : constant Any_Local_Offset := Any_Local_Offset
              (60 * Hour_Offset + Minute_Offset);
            Offset          : constant Any_Local_Offset :=
              (if Positive_Offset
               then Absolute_Offset
               else -Absolute_Offset);
            Offset_Datetime : constant Any_Offset_Datetime :=
              (Datetime       => Datetime,
               Offset         => Offset,
               Unknown_Offset => Offset = 0 and then not Positive_Offset);
         begin
            Token_Buffer.Token :=
              (Kind                  => Offset_Datetime_Literal,
               Offset_Datetime_Value => Offset_Datetime);
            return True;
         end;
      end;
   end Read_Local_Time;

   ----------------
   -- Read_Float --
   ----------------

   function Read_Float
     (Positive      : Boolean;
      Integer_Value : Interfaces.Unsigned_64) return Boolean
   is
      function Invalid_Float return Boolean is
        (Create_Lexing_Error ("invalid float"));
      function Too_Large return Boolean is
        (Create_Lexing_Error ("too large float"));

      function Read_Simple_Integer
        (Value : out Interfaces.Unsigned_64) return Boolean
         with Pre => not Codepoint_Buffer.EOF;
      --  Helper to read a simple decimal integer, including underscores

      -------------------------
      -- Read_Simple_Integer --
      -------------------------

      function Read_Simple_Integer
        (Value : out Interfaces.Unsigned_64) return Boolean
      is
         use type Interfaces.Unsigned_64;

         Empty : Boolean := True;
         --  Whether we read no digit so far

         Previous_Is_Underscore : Boolean := True;
         --  Given that it is forbidden to have a leading underscore, do as if
         --  we just had an underscore at the beginning so that we need to
         --  check only for consecutive underscores.
      begin
         Value := 0;

         --  Read and decode all digits that follow in the stream

         while not Codepoint_Buffer.EOF
               and then Codepoint_Buffer.Codepoint in '0' .. '9' | '_'
         loop
            --  Discard underscores, yet do not allow consecutive ones

            if Codepoint_Buffer.Codepoint = '_' then
               if Previous_Is_Underscore then
                  return Invalid_Float;
               else
                  Previous_Is_Underscore := True;
               end if;

            --  Decode digits to build Value as we go. Consider the number too
            --  large as soon as it is bigger than half the largest possible
            --  unsigned 64 value: bigger numbers are unlikely and this allows
            --  us to negate values safely later on.

            else
               begin
                  Value :=
                     10 * Value + Digit_Value (Codepoint_Buffer.Codepoint);

                  if Value > Interfaces.Unsigned_64'Last / 2 then
                     return Too_Large;
                  end if;

               exception
                  when Constraint_Error =>
                     return Too_Large;
               end;
               Empty := False;
               Previous_Is_Underscore := False;
            end if;

            if not Read_Codepoint then
               return Invalid_Float;
            end if;
         end loop;

         --  We expect at least one digit, and the last codepoint must not be
         --  an underscore.

         if Empty or else Previous_Is_Underscore then
            return Invalid_Float;
         else
            return True;
         end if;
      end Read_Simple_Integer;

      Fractional_Value  : Interfaces.Unsigned_64 := 0;
      Exponent          : Interfaces.Unsigned_64 := 0;
      Exponent_Positive : Boolean := True;
      Result            : Any_Float := (Kind => Regular, Value => <>);
   begin
      --  Read the fractional part, if present

      if Codepoint_Buffer.Codepoint = '.' then
         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF then
            return Invalid_Float;
         elsif not Read_Simple_Integer (Fractional_Value) then
            return False;
         end if;
      end if;

      --  Read the exponent, if present

      if not Codepoint_Buffer.EOF
         and then Codepoint_Buffer.Codepoint in 'e' | 'E'
      then
         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF then
            return Invalid_Float;
         end if;

         --  First read the sign

         if Codepoint_Buffer.Codepoint in '+' | '-' then
            Exponent_Positive := Codepoint_Buffer.Codepoint /= '-';
            if not Read_Codepoint then
               return False;
            elsif Codepoint_Buffer.EOF then
               return Invalid_Float;
            end if;
         end if;

         --  Then the exponent absolute value

         if not Read_Simple_Integer (Exponent) then
            return False;
         end if;
      end if;

      --  Reemit the last codepoint: it is not part of the float token and the
      --  next token reading iteration needs to read it.

      Reemit_Codepoint;

      --  Use all lexed parts to create a floating point value

      declare
         function Image
           (Positive : Boolean; Value : Interfaces.Unsigned_64) return String;

         -----------
         -- Image --
         -----------

         function Image
           (Positive : Boolean; Value : Interfaces.Unsigned_64) return String
         is
            Result   : constant String := Value'Image;
            Stripped : String renames Result (Result'First + 1 .. Result'Last);
            pragma Assert (Result (Result'First) = ' ');
         begin
            return (if Positive
                    then Stripped
                    else "-" & Stripped);
         end Image;

         Value_Image : constant String :=
            Image (Positive, Integer_Value)
            & "." & Image (True, Fractional_Value)
            & "e" & Image (Exponent_Positive, Exponent);
      begin
         Result.Value := Valid_Float'Value (Value_Image);
      exception
         when Constraint_Error =>
            return Too_Large;
      end;
      if not Result.Value'Valid then
         if Result.Value < Valid_Float'First then
            Result := (Kind => Infinity, Positive => False);
         elsif Result.Value > Valid_Float'Last then
            Result := (Kind => Infinity, Positive => True);
         else
            Result := (Kind => NaN, Positive => True);
         end if;
      end if;
      Token_Buffer.Token := (Kind => Float_Literal, Float_Value => Result);
      return True;
   end Read_Float;

   ------------------------
   -- Read_Special_Float --
   ------------------------

   function Read_Special_Float
     (Name     : Wide_Wide_String;
      Kind     : Special_Float_Kind;
      Positive : Boolean) return Boolean
   is
      function Invalid_Float return Boolean is
        (Create_Lexing_Error ("invalid float"));
   begin
      pragma Assert (Codepoint_Buffer.Codepoint = Name (Name'First));

      for C of Name (Name'First + 1 .. Name'Last) loop
         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF
            or else Codepoint_Buffer.Codepoint /= C
         then
            return Invalid_Float;
         end if;
      end loop;

      declare
         Value : Any_Float (Kind);
      begin
         Value.Positive := Positive;
         Token_Buffer.Token := (Kind => Float_Literal, Float_Value => Value);
      end;
      return True;
   end Read_Special_Float;

   -------------------
   -- Read_Bare_Key --
   -------------------

   function Read_Bare_Key return Boolean is
   begin
      Token_Buffer.Token :=
        (Kind          => String_Literal,
         String_Format => Bare_Key,
         others        => <>);

      loop
         --  Add the previously read character to the key token

         Append_As_UTF8 (Codepoint_Buffer.Codepoint);

         --  Then check the next character: exit the loop as soon as we either
         --  reach the end of stream, or we find a non-key character.

         if not Read_Codepoint then
            return False;
         end if;

         exit when
            Codepoint_Buffer.EOF
            or else Codepoint_Buffer.Codepoint not in
               '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' | '-';
      end loop;

      --  Be sure to schedule the re-emission of the read event that made us
      --  stop reading characters so that callers can attempt to read one more
      --  token.

      Reemit_Codepoint;

      return True;
   end Read_Bare_Key;

   ------------------
   -- Read_Keyword --
   ------------------

   function Read_Keyword
     (Text : Wide_Wide_String; Token : Any_Token) return Boolean is
   begin
      --  Read all codepoints that constitute the token (except the first one,
      --  as per the Read_Keyword contract) and make sure they match the
      --  expected text.

      for I in Text'First + 1 .. Text'Last loop
         if not Read_Codepoint then
            return False;
         elsif Codepoint_Buffer.EOF
               or else Codepoint_Buffer.Codepoint /= Text (I)
         then
            return Create_Lexing_Error ("invalid token");
         end if;
      end loop;

      Token_Buffer.Token := Token;
      return True;
   end Read_Keyword;

   ----------------
   -- Parse_Line --
   ----------------

   function Parse_Line return Boolean is
   begin
      if not Read_Token (Key_Expected => True) then
         return False;
      elsif Token_Buffer.EOF then
         return True;
      end if;

      case Token_Buffer.Token.Kind is
         when Newline =>
            return True;

         when Square_Bracket_Open =>
            --  Parse a [section]

            return Parse_Section (Array_Of_Table => False);

         when Double_Square_Bracket_Open =>
            --  Parse a [[section]]

            return Parse_Section (Array_Of_Table => True);

         when String_Literal =>
            --  Parse a key/value pair

            declare
               Table : TOML_Value := Current_Table;
               Key   : Unbounded_UTF8_String;
               Value : TOML_Value;
            begin
               --  Parse the dotted key that identify the table entry to
               --  create. In particular, get the destination table (Table) and
               --  the key to insert (Key) and make sure there is no existing
               --  entry for Key.

               if not Parse_Dotted_Keys (Table, Key, Traverse_Arrays => False)
               then
                  return False;
               elsif Table.Has (Key) then
                  return Create_Syntax_Error ("duplicate key");
               end if;

               if Token_Buffer.EOF or else Token_Buffer.Token.Kind /= Equal
               then
                  return Create_Syntax_Error;
               end if;

               --  Now parse the value for the entry to insert. On success, do
               --  the insertion.

               if Parse_Value (Value) then
                  Table.Set (Key, Value);
               else
                  return False;
               end if;

               --  Expect either a new line, or the end of the file

               if not Read_Token (Key_Expected => True) then
                  return False;
               elsif Token_Buffer.EOF then
                  return True;
               elsif Token_Buffer.Token.Kind /= Newline then
                  return Create_Syntax_Error ("missing newline");
               end if;

               return True;
            end;

         when others =>
            return Create_Syntax_Error;
      end case;
   end Parse_Line;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table
     (Key             : Unbounded_UTF8_String;
      Table           : in out TOML_Value;
      Traverse_Arrays : Boolean := False) return Boolean
   is
      Next_Table : TOML_Value := Table.Get_Or_Null (Key);
   begin
      if Next_Table.Is_Null then
         Next_Table := Create_Table;
         Next_Table.Set_Implicitly_Created;
         Table.Set (Key, Next_Table);
         Table := Next_Table;
         return True;

      elsif Next_Table.Kind = TOML_Table then
         Table := Next_Table;
         return True;

      elsif Traverse_Arrays
            and then Next_Table.Kind = TOML_Array
            and then Next_Table.Length > 0
      then
         Table := Next_Table.Item (Next_Table.Length);
         return True;

      else
         return Create_Syntax_Error ("invalid table key");
      end if;
   end Get_Table;

   -------------------
   -- Parse_Section --
   -------------------

   function Parse_Section (Array_Of_Table : Boolean) return Boolean is
      Opening_Bracket_Location : constant Source_Location :=
         Token_Buffer.Location;

      Closing_Bracket : constant Token_Kind :=
        (if Array_Of_Table
         then Double_Square_Bracket_Close
         else Square_Bracket_Close);

      Table : TOML_Value := Root_Table;
      Key   : Unbounded_UTF8_String;
   begin
      --  Get the first token for the section name, as per Parse_Dotted_Keys's
      --  contract.

      if not Read_Token (Key_Expected => True) then
         return False;

      elsif Token_Buffer.EOF then
         return Create_Syntax_Error;

      elsif not Parse_Dotted_Keys (Table, Key, Traverse_Arrays => True) then
         return False;

      --  At this point, Parse_Dotted_Keys left the first non-key token in
      --  Token_Buffer: make sure we have the closing bracket.

      elsif Token_Buffer.EOF or else Token_Buffer.Token.Kind /= Closing_Bracket
      then
         return Create_Syntax_Error;

      --  And now, make sure that we have either EOF or a newline next

      elsif not Read_Token (Key_Expected => False)
            or else (not Token_Buffer.EOF
                     and then Token_Buffer.Token.Kind /= Newline)
      then
         return Create_Syntax_Error;
      end if;

      --  Finally, create the requested table

      if Array_Of_Table then
         --  Key is supposed to refer to an array of tables: if there is no
         --  such entry in Table, create one, otherwise make sure it has the
         --  expected item type.

         declare
            Arr : TOML_Value := Table.Get_Or_Null (Key);
         begin
            if Arr.Is_Null then
               Arr := Create_Array;
               Arr.Set_Implicitly_Created;
               Table.Set (Key, Arr);
            elsif Arr.Kind /= TOML_Array then
               return Create_Error
                 ("invalid array", Opening_Bracket_Location);
            elsif not Arr.Implicitly_Created then
               return Create_Error
                 ("arrays of tables cannot complete inline arrays",
                  Opening_Bracket_Location);
            end if;

            --  Create a new table and append it to this array

            Current_Table := Create_Table;
            Arr.Append (Current_Table);
         end;

      else
         --  If Key is already associated to a table, return it (it's an error
         --  if it is not a table or if it was already created explicitly).
         --  Create the destination table otherwise.

         if Table.Has (Key) then
            Current_Table := Table.Get (Key);
            if Current_Table.Kind /= TOML_Table then
               return Create_Error
                 ("duplicate key", Opening_Bracket_Location);
            elsif Current_Table.Implicitly_Created then
               Current_Table.Set_Explicitly_Created;
            else
               return Create_Error
                 ("cannot create tables twice", Opening_Bracket_Location);
            end if;
         else
            Current_Table := Create_Table;
            Table.Set (Key, Current_Table);
         end if;
      end if;

      return True;
   end Parse_Section;

   -----------------------
   -- Parse_Dotted_Keys --
   -----------------------

   function Parse_Dotted_Keys
     (Table           : in out TOML_Value;
      Key             : out Unbounded_UTF8_String;
      Traverse_Arrays : Boolean := False) return Boolean
   is
      Has_Key : Boolean := False;
      --  Whether we parsed at least one key
   begin
      loop
         --  Process the current key, updating Table accordingly

         if Token_Buffer.EOF
            or else Token_Buffer.Token.Kind /= String_Literal
            or else Token_Buffer.Token.String_Format not in Valid_Key_Format
         then
            return Create_Syntax_Error;
         end if;

         --  We are about to parse a key. If we already parsed one, we need to
         --  fetch the corresponding table.

         if Has_Key and then not Get_Table (Key, Table, Traverse_Arrays) then
            return False;
         end if;
         Key := Token_Buffer.Token.String_Value;
         Has_Key := True;

         --  If the next token is a dot, expect another key. Otherwise, stop
         --  parsing keys.

         if not Read_Token (Key_Expected => True) then
            return False;

         elsif Token_Buffer.EOF or else Token_Buffer.Token.Kind /= Dot then
            return True;

         elsif not Read_Token (Key_Expected => True) then
            return False;
         end if;
      end loop;
   end Parse_Dotted_Keys;

   -----------------
   -- Parse_Value --
   -----------------

   function Parse_Value (Value : out TOML_Value) return Boolean is
   begin
      --  Fetch the first token that encodes the value to parse...

      if not Read_Token (Key_Expected => False) then
         return False;

      elsif Token_Buffer.EOF then
         return Create_Syntax_Error;
      end if;

      case Token_Buffer.Token.Kind is
         when Boolean_Literal =>
            Value := Create_Boolean (Token_Buffer.Token.Boolean_Value);

         when Integer_Literal =>
            Value := Create_Integer (Token_Buffer.Token.Integer_Value);

         when Float_Literal =>
            Value := Create_Float (Token_Buffer.Token.Float_Value);

         when String_Literal =>
            Value := Create_String (Token_Buffer.Token.String_Value);

         when Offset_Datetime_Literal =>
            Value := Create_Offset_Datetime
              (Token_Buffer.Token.Offset_Datetime_Value);

         when Local_Datetime_Literal =>
            Value := Create_Local_Datetime
              (Token_Buffer.Token.Local_Datetime_Value);

         when Local_Date_Literal =>
            Value := Create_Local_Date (Token_Buffer.Token.Local_Date_Value);

         when Local_Time_Literal =>
            Value := Create_Local_Time (Token_Buffer.Token.Local_Time_Value);

         when Square_Bracket_Open =>
            return Parse_Array (Value);

         when Curly_Bracket_Open =>
            return Parse_Table (Value);

         when others =>
            return Create_Syntax_Error
              ("invalid (or not supported yet) syntax");
      end case;

      return True;
   end Parse_Value;

   -----------------
   -- Parse_Array --
   -----------------

   function Parse_Array (Value : out TOML_Value) return Boolean is
      Comma_Allowed : Boolean := False;
   begin
      Value := Create_Array;

      loop
         --  Fetch the next token. We need one, so reaching end of stream is a
         --  parsing error.

         if not Read_Token (Key_Expected => False) then
            return False;

         elsif Token_Buffer.EOF then
            return Create_Syntax_Error;
         end if;

         case Token_Buffer.Token.Kind is
            when Square_Bracket_Close =>
               return True;

            when Newline =>
               --  Newlines are allowed anywhere between surrounding brackets,
               --  values and commas.
               null;

            when Comma =>
               if Comma_Allowed then
                  Comma_Allowed := False;
               else
                  return Create_Syntax_Error;
               end if;

            when others =>
               --  We are expecting a comma right after parsing a value, so if
               --  we have a potential value in this case, we know a comma is
               --  missing.

               if Comma_Allowed then
                  return Create_Syntax_Error;
               end if;

               --  We already read the first token for this value, but
               --  Parse_Value expects it not to be read yet, so plan to
               --  re-emit it.

               Reemit_Token;

               declare
                  Item : TOML_Value;
               begin
                  --  Parse the item value and then append the item to the
                  --  result.

                  if not Parse_Value (Item) then
                     return False;
                  end if;

                  Value.Append (Item);
               end;
               Comma_Allowed := True;
         end case;
      end loop;
   end Parse_Array;

   -----------------
   -- Parse_Table --
   -----------------

   function Parse_Table (Value : out TOML_Value) return Boolean is
      Comma_Allowed  : Boolean := False;
      Last_Was_Comma : Boolean := False;
      Key            : Unbounded_UTF8_String;
      Table          : TOML_Value;
   begin
      Value := Create_Table;

      loop
         --  Fetch the next token (a potential key for the next table entry, or
         --  a closing bracket). We need one, so reaching end of stream is a
         --  parsing error.

         if not Read_Token (Key_Expected => True) then
            return False;
         elsif Token_Buffer.EOF then
            return Create_Syntax_Error;
         end if;

         case Token_Buffer.Token.Kind is
            when Curly_Bracket_Close =>
               if Last_Was_Comma then
                  return Create_Error ("invalid trailing comma");
               end if;
               return True;

            when Newline =>
               return Create_Error ("newlines not allowed in inlined tables");

            when Comma =>
               Last_Was_Comma := True;
               if Comma_Allowed then
                  Comma_Allowed := False;
               else
                  return Create_Syntax_Error;
               end if;

            when String_Literal =>

               --  We are expecting a comma right after parsing a value, so if
               --  we have a potential value in this case, we know a comma is
               --  missing.

               if Comma_Allowed then
                  return Create_Syntax_Error;
               end if;

               --  Dotted keys are allowed in inline tables, so attempt to
               --  parse dotted keys (i.e. multiple tokens) and make sure it is
               --  followed by an equal token.

               Table := Value;
               if not Parse_Dotted_Keys (Table, Key, Traverse_Arrays => False)
               then
                  return False;
               elsif Token_Buffer.EOF or else Token_Buffer.Token.Kind /= Equal
               then
                  return Create_Syntax_Error;
               end if;

               --  Then try to create the table entry

               if Table.Has (Key) then
                  return Create_Syntax_Error ("duplicate key");
               end if;

               --  Now read the value

               declare
                  Item : TOML_Value;
               begin
                  --  Parse the item value, reject heterogeneous arrays, and
                  --  then append the item to the result.

                  if not Parse_Value (Item) then
                     return False;
                  end if;

                  --  Finally register the table entry

                  Table.Set (Key, Item);
                  Comma_Allowed := True;
                  Last_Was_Comma := False;
               end;

            when others =>
               return Create_Syntax_Error;
         end case;
      end loop;
   end Parse_Table;

begin
   while not Token_Buffer.EOF loop
      if not Parse_Line then
         return Result;
      end if;
   end loop;

   return Result;
end TOML.Generic_Parse;
