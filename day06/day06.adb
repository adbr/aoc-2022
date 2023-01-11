-- adbr [2022-12-25 Sun]
-- Usage: day06 filename

with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;

procedure Day06 is
   use Ada.Text_IO;
   use Ada.Text_IO.Unbounded_IO;
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;
   
   Marker_Error : exception;
   
   procedure Detect_Marker
     (Buffer      : in Unbounded_String;
      Marker_Size : in Positive;
      Detected    : out Boolean;
      Position    : out Positive)
   is

      function Is_Marker (S : String) return Boolean is
      begin
         for C of S loop
            declare
               N : Natural := 0;
            begin
               for C1 of S loop
                  if C = C1 then
                     N := N + 1;
                  end if;
               end loop;
               if N > 1 then
                  return False;
               end if;
            end;
         end loop;
         return True;
      end Is_Marker;

   begin
      Detected := False;
      for I in 1 .. Length (Buffer) loop
         if I >= Marker_Size then
            if Is_Marker (Slice (Buffer, I - Marker_Size + 1, I)) then
               Detected := True;
               Position := I;
               return;
            end if;
         end if;
      end loop;
   end Detect_Marker;
   
   procedure Process_Stream (Filename : String) is
      File     : File_Type;
      Buffer   : Unbounded_String;
      Detected : Boolean;
      Position : Positive;
      Packet_Marker_Size  : constant Positive := 4;
      Message_Marker_Size : constant Positive := 14;
   begin
      Open (File, In_File, Filename);
      Get_Line (File, Buffer);
      Close (File);
      
      Detect_Marker (Buffer, Packet_Marker_Size, Detected, Position);
      if Detected then
         Put_Line ("Part 1: Packet marker position:" & Position'Image);
      else
         raise Marker_Error with "Marker not found";
      end if;
      
      Detect_Marker (Buffer, Message_Marker_Size, Detected, Position);
      if Detected then
         Put_Line ("Part 2: Message marker position:" & Position'Image);
      else
         raise Marker_Error with "Marker not found";
      end if;
   end Process_Stream;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: day06 filename");
      Set_Exit_Status (Failure);
      return;
   end if;
   Process_Stream (Argument (1));
end Day06;

-- ./bin/day06 input.txt 
-- Part 1: Packet marker position: 1816
-- Part 2: Message marker position: 2625
