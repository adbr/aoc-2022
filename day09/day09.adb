--  adbr [2023-01-18 Wed]
--  Usage: day09 filename

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers;

procedure Day09 is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Containers;
   package CL renames Ada.Command_Line;

   Input_Error : exception;

   type Direction_Type is (Left, Right, Up, Down);

   type Move_Type is
      record
         Direction : Direction_Type;
         Steps     : Positive;
      end record;

   package Move_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Move_Type);
   subtype Move_Vector is Move_Vectors.Vector;

   type Position_Type is
      record
         X : Integer;
         Y : Integer;
      end record;

   type Rope_Type is
      record
         Head : Position_Type;
         Tail : Position_Type;
      end record;
   
   -- Position_Map type:
   
   function Hash (Key : Position_Type) return Hash_Type is
   begin
      return Hash_Type (abs (Key.X * Key.Y + Key.X + Key.Y));
   end Hash;

   function Equivalent_Keys (Left, Right : Position_Type) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Keys;

   package Position_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Position_Type,
      Element_Type    => Positive,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);
   subtype Position_Map is Position_Maps.Map;

   -----------------
   -- Print_Moves --
   -----------------

   procedure Print_Moves (Moves : in Move_Vector) is
   begin
      for M of Moves loop
         Put_Line ("[" & M.Direction'Image & " =>" & M.Steps'Image & "]");
      end loop;
   end Print_Moves;

   -----------
   -- Part1 --
   -----------

   procedure Part1 (Moves : in Move_Vector) is

      ------------------
      -- Are_Touching --
      ------------------

      function Are_Touching (P1, P2 : Position_Type) return Boolean is
      begin
         return abs (P1.X - P2.X) < 2 and abs (P1.Y - P2.Y) < 2;
      end Are_Touching;

      ---------------
      -- Move_Rope --
      ---------------

      procedure Move_Rope
        (Rope      : in out Rope_Type;
         Direction : in Direction_Type)
      is
         Old_Head : Position_Type := Rope.Head;
      begin
         case Direction is
            when Left =>
               Rope.Head.X := Rope.Head.X - 1;
            when Right =>
               Rope.Head.X := Rope.Head.X + 1;
            when Up =>
               Rope.Head.Y := Rope.Head.Y + 1;
            when Down =>
               Rope.Head.Y := Rope.Head.Y - 1;
         end case;
         if not Are_Touching (Rope.Head, Rope.Tail) then
            Rope.Tail := Old_Head;
         end if;
      end Move_Rope;

      ------------------
      -- Add_Position --
      ------------------

      procedure Add_Position
        (Positions : in out Position_Map;
         P  : Position_Type)
      is
      begin
         if Positions.Contains (P) then
            declare
               N : Positive := Positions.Element (P) + 1;
            begin
               Positions.Replace (P, N);
            end;
         else
            Positions.Insert (P, 1);
         end if;
      end Add_Position;

      -- Local variables

      Rope : Rope_Type := (Head => (1, 1), Tail => (1, 1));
      Positions : Position_Map;  -- Tail positions

   begin
      Add_Position (Positions, Rope.Tail);
      for M of Moves loop
         for S in 1 .. M.Steps loop
            Move_Rope (Rope, M.Direction);
            Add_Position (Positions, Rope.Tail);
         end loop;
      end loop;
      Put_Line ("Part 1: Tail positions wisited at least once:" &
                  Positions.Length'Image);
   end Part1;

   ---------------
   -- Read_Data --
   ---------------

   procedure Read_Data (Filename : String; Moves : in out Move_Vector) is
      File : File_Type;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         declare
            Direction : Character;
            Steps     : Positive;
            Move      : Move_Type;
         begin
            Get (File, Direction);
            Get (File, Steps);
            case Direction is
               when 'L' => Move.Direction := Left;
               when 'R' => Move.Direction := Right;
               when 'U' => Move.Direction := Up;
               when 'D' => Move.Direction := Down;
               when others =>
                  raise Input_Error
                    with "Unknown direction: '" & Direction & "'" &
                      " in line:" & Count'Image (Line (File));
            end case;
            Move.Steps := Steps;
            Moves.Append (Move);
            Skip_Line (File);
         end;
      end loop;
      Close (File);
   end Read_Data;

begin
   if CL.Argument_Count /= 1 then
      Put_Line ("Usage: day09 filename");
      CL.Set_Exit_Status (CL.Failure);
      return;
   end if;

   declare
      Moves    : Move_Vector;
      Filename : constant String := CL.Argument (1);
   begin
      Read_Data (Filename, Moves);
      Part1 (Moves);
   end;
end Day09;

-- ./bin/day09 input.txt 
-- Part 1: Tail positions wisited at least once: 6384
