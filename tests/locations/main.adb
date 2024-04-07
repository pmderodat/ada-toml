with Ada.Strings.Unbounded;
with Ada.Text_IO;

with TOML;
with TOML.File_IO;

procedure Main is
   package TIO renames Ada.Text_IO;

   procedure Process (Value : TOML.TOML_Value; Prefix : String);

   -------------
   -- Process --
   -------------

   procedure Process (Value : TOML.TOML_Value; Prefix : String) is
      K : constant TOML.Any_Value_Kind := Value.Kind;
   begin
      TIO.Put (Prefix & "* " & K'Image);
      if Value.Location.Line = 0 then
         TIO.Put_Line (" [no location]");
      else
         TIO.Put_Line (" at " & TOML.Format_Location (Value.Location));
      end if;
      case K is
         when TOML.TOML_Table =>
            for E of Value.Iterate_On_Table loop
               TIO.Put_Line
                 (Prefix
                  & "  " & Ada.Strings.Unbounded.To_String (E.Key) & ":");
               Process (E.Value, Prefix & "    ");
            end loop;

         when TOML.TOML_Array =>
            for I in 1 .. Value.Length loop
               Process (Value.Item (I), Prefix & "    ");
            end loop;

         when others =>
            null;
      end case;
   end Process;

   Value : constant TOML.TOML_Value :=
      TOML.File_IO.Load_File ("example.toml").Value;
begin
   Process (Value, "");
end Main;
