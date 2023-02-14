--  adbr [2023-02-12 Sun]
--  Usage: day10 filename

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;

procedure Day10 is
   use Ada.Text_IO;
   package CL renames Ada.Command_Line;
   
   Input_Error : exception;
   
   type Instruction_Kind is (Noop, Addx);
   
   Instruction_Cycles : constant array (Instruction_Kind) of Positive :=
     (Noop => 1, Addx => 2);
   
   type Instruction_Type (Kind : Instruction_Kind) is
      record
         case Kind is
            when Addx =>
               Argument : Integer;
            when Noop =>
               null;
         end case;
      end record;
   
   package Instruction_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Instruction_Type);
   subtype Instruction_Vector is Instruction_Vectors.Vector;
   
   type Signal_Strength_Type is
      record
         Cycle : Positive;
         Value : Integer;
      end record;
   
   Signal_Strengths : array (Positive range <>) of Signal_Strength_Type :=
     ((Cycle =>  20, Value => 0),
      (Cycle =>  60, Value => 0),
      (Cycle => 100, Value => 0),
      (Cycle => 140, Value => 0),
      (Cycle => 180, Value => 0),
      (Cycle => 220, Value => 0));
   
   -----------
   -- Part1 --
   -----------
   
   procedure Part1 (Instructions : Instruction_Vector) is
      Cycles    : Natural := 0;
      RegisterX : Integer := 1;
      
      ----------------------------
      -- Update_Signal_Strength --
      ----------------------------
      
      procedure Update_Signal_Strength is
      begin
         for S of Signal_Strengths loop
            if Cycles = S.Cycle then
               S.Value := S.Cycle * RegisterX;
            end if;
         end loop;
      end Update_Signal_Strength;
      
   begin
      for I of Instructions loop
         case I.Kind is
            when Noop =>
               Cycles := Cycles + 1;
               Update_Signal_Strength;
            when Addx =>
               for C in 1 .. Instruction_Cycles (I.Kind) loop
                  Cycles := Cycles + 1;
                  Update_Signal_Strength;
               end loop;
               RegisterX := RegisterX + I.Argument;
         end case;
      end loop;
      
      declare
         Sum : Integer := 0;
      begin
         for S of Signal_Strengths loop
            Sum := Sum + S.Value;
         end loop;
         Put_Line ("Part 1: sum of six signal strengths:" & Sum'Image);
      end;
   end Part1;
   
   ---------------
   -- Read_Data --
   ---------------
   
   procedure Read_Data
     (Filename     : String;
      Instructions : in out Instruction_Vector)
   is
      use Ada.Integer_Text_IO;
      File        : File_Type;
      Instruction : String (1 .. 4);
      Separator   : Character;
      Argument    : Integer;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Get (File, Instruction);
         if Instruction = "noop" then
            declare
               I : Instruction_Type (Kind => Noop);
            begin
               Instructions.Append (I);
            end;
         elsif Instruction = "addx" then
            Get (File, Separator);
            Get (File, Argument);
            declare
               I : Instruction_Type (Kind => Addx);
            begin
               I.Argument := Argument;
               Instructions.Append (I);
            end;
         else
            raise Input_Error
              with "Invalid instruction: '" & Instruction & "'";
         end if;
         Skip_Line (File);
      end loop;
      Close (File);
   end Read_Data;
   
begin
   if CL.Argument_Count /= 1 then
      Put_Line ("Usage: day10 filename");
      CL.Set_Exit_Status (CL.Failure);
      return;
   end if;
   
   declare
      Filename     : String := CL.Argument (1);
      Instructions : Instruction_Vector;
   begin
      Read_Data (Filename, Instructions);
      Part1 (Instructions);
   end;
end Day10;

-- ./bin/day10 input.txt 
-- Part 1: sum of six signal strengths: 13860
