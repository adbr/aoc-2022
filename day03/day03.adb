-- adbr [2022-12-04 Sun]

with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;

procedure Day03 is
   use Ada.Text_IO;
   use Ada.Text_IO.Unbounded_IO;
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;

   subtype Priority_Type is Integer range 1 .. 52;
   type Group_Type is record
      Rucksack1 : Unbounded_String;
      Rucksack2 : Unbounded_String;
      Rucksack3 : Unbounded_String;
   end record;

   Value_Error : exception;

   function Priority (I : Character) return Priority_Type
   with Pre => I in 'a' .. 'z' or I in 'A' .. 'Z'       -- flaga -gnata
   is
      Result : Priority_Type;
   begin
      case I is
         when 'a' .. 'z' =>
            -- a .. z => 1 .. 26
            Result := Character'Pos (I) - Character'Pos ('a') + 1;
         when 'A' .. 'Z' =>
            -- A .. Z => 27 .. 52
            Result := Character'Pos (I) - Character'Pos ('A') + 27;
         when others =>
            raise Value_Error with "Wrong item symbol: " & I;
      end case;
      return Result;
   end Priority;

   function Wrong_Items (List : String) return String is
      I      : Positive := List'Length / 2;
      Part1  : String   := List (List'First .. I);
      Part2  : String   := List (I + 1 .. List'Last);
      Result : Unbounded_String := To_Unbounded_String ("");

      function Find (List : String; C : Character) return Boolean is
      begin
         for D of List loop
            if C = D then
               return True;
            end if;
         end loop;
         return False;
      end Find;

   begin
      for C of Part1 loop
         if Find (Part2, C) then
            if not Find (To_String (Result), C) then
               Append (Result, C);
            end if;
         end if;
      end loop;
      return To_String (Result);
   end Wrong_Items;
   
   function Group_Badge (Group : Group_Type) return Character is
      
      function Find (List : Unbounded_String; C : Character) return Boolean is
      begin
         for I in 1 .. Length (List) loop
            if Element (List, I) = C then
               return True;
            end if;
         end loop;
         return False;
      end Find;
      
      Result : Character;
      C      : Character;

   begin
      for I in 1 .. Length (Group.Rucksack1) loop
         C := Element (Group.Rucksack1, I);
         if Find (Group.Rucksack2, C) and Find (Group.Rucksack3, C) then
            Result := C;
            exit;
         end if;
      end loop;
      return Result;
   end Group_Badge;

   procedure Part1 (Filename : String) is
      File : File_Type;
      Sum  : Natural := 0; -- suma priorytetów błędnych elementów
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         declare
            Line  : String := Get_Line (File);
            Wrong : String := Wrong_Items (Line);
         begin
            for C of Wrong loop
               Sum := Sum + Priority (C);
            end loop;
         end;
      end loop;
      Close (File);
      Put_Line ("Part1:");
      Put_Line ("  Sum of priorities: " & Sum'Image);
   end Part1;

   procedure Part2 (Filename : String) is
      File  : File_Type;
      Group : Group_Type;
      Sum   : Natural := 0; -- suma priorytetów badges items
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Group.Rucksack1 := Get_Line (File);
         Group.Rucksack2 := Get_Line (File);
         Group.Rucksack3 := Get_Line (File);
         Sum := Sum + Priority (Group_Badge (Group));
      end loop;
      Close (File);
      Put_Line ("Part2:");
      Put_Line ("  Sum of priorities of badges items: " & Sum'Image);
   end Part2;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: day03 filename");
      Set_Exit_Status (Failure);
      return;
   end if;

   Part1 (Argument (1));
   Part2 (Argument (1));
end Day03;

-- ./bin/day03 input.txt 
-- Part1:
--   Sum of priorities:  7701
-- Part2:
--   Sum of priorities of badges items:  2644
