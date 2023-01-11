--  adbr [2022-12-01 Thu]
--
--  Usage: day01 filename

with Ada.Text_IO;
with Ada.Command_Line;

procedure Day01 is
   use Ada.Text_IO;
   use Ada.Command_Line;

   type Calories_Type is new Natural;

   package Calories_Text_IO is new Ada.Text_IO.Integer_IO
     (Num => Calories_Type);
   use Calories_Text_IO;

   procedure Part1 (Filename : String) is
      File    : File_Type;
      Cal     : Calories_Type;
      Cal_Sum : Calories_Type := 0;
      Cal_Max : Calories_Type := 0;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Get (File, Cal);
         Skip_Line (File);
         Cal_Sum := Cal_Sum + Cal;

         if End_Of_Line (File) then
            --  end of group
            if Cal_Sum > Cal_Max then
               Cal_Max := Cal_Sum;
            end if;
            Cal_Sum := 0;
         end if;
      end loop;
      Close (File);

      Put_Line ("Part 1:");
      Put_Line ("  Max calories: " & Cal_Max'Img);
   end Part1;

   procedure Part2 (Filename : String) is
      File      : File_Type;
      Cal       : Calories_Type;
      Cal_Sum   : Calories_Type := 0;
      Cal_Max_1 : Calories_Type := 0;
      Cal_Max_2 : Calories_Type := 0;
      Cal_Max_3 : Calories_Type := 0;
      Cal_Total : Calories_Type;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Get (File, Cal);
         Skip_Line (File);
         Cal_Sum := Cal_Sum + Cal;

         if End_Of_Line (File) then
            --  end of group
            if Cal_Sum >= Cal_Max_1 then
               Cal_Max_2 := Cal_Max_1;
               Cal_Max_1 := Cal_Sum;
            elsif Cal_Sum >= Cal_Max_2 then
               Cal_Max_3 := Cal_Max_2;
               Cal_Max_2 := Cal_Sum;
            elsif Cal_Sum >= Cal_Max_3 then
               Cal_Max_3 := Cal_Sum;
            end if;
            Cal_Sum := 0;
         end if;
      end loop;
      Close (File);

      Cal_Total := Cal_Max_1 + Cal_Max_2 + Cal_Max_3;
      Put_Line ("Part 2:");
      Put_Line ("  Total three max calories: " & Cal_Total'Img);
   end Part2;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: day01 filename");
      Set_Exit_Status (Failure);
      return;
   end if;

   Part1 (Argument (1));
   Part2 (Argument (1));
end Day01;

-- ./bin/day01 input.txt
-- Part 1:
--   Max calories:  67450
-- Part 2:
--   Total three max calories:  199357
