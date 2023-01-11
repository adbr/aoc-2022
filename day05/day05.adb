-- adbr [2022-12-08 Thu]

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

procedure Day05 is
   use Ada.Text_IO;
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;

   subtype Crate_String is String (1 .. 3);

   type Move_Type is record
      Move : Positive;
      From : Positive;
      To   : Positive;
   end record;

   package Move_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Move_Type);
   subtype Moves_Type is Move_Vectors.Vector;

   package Stack_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Character);
   subtype Stack_Type is Stack_Vectors.Vector;

   package Heap_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Stack_Type,
      "="          => Stack_Vectors."=");
   subtype Heap_Type is Heap_Vectors.Vector;

   procedure Print_Heap (Heap : in Heap_Type) is
   begin
      for I in Heap.First_Index .. Heap.Last_Index loop
         Put (I'Image & ":");
         for C of Heap.Element (I) loop
            Put (C);
         end loop;
         New_Line;
      end loop;
   end Print_Heap;

   procedure Add_Crate
     (Heap         : in out Heap_Type;
      Stack_Number : in     Positive;  -- Stack index in Heap
      Crate        : in     Character) -- Crate character symbol
   is
      use Ada.Containers;
      use Stack_Vectors;
      use Heap_Vectors;
   begin
      -- Dodanie brakujących pustych stosów
      while Count_Type (Stack_Number) > Heap.Length loop
         Heap.Append (Stack_Vectors.To_Vector (0));
      end loop;

      -- Modyfikacja stosu
      Heap.Reference (Stack_Number).Prepend (Crate);

      -- Działa też taki sposób:
      -- declare
      --    Stack : access Stack_Type := Heap.Reference (Stack_Number).Element;
      -- begin
      --    Stack.Prepend (Crate);
      -- end;
   end Add_Crate;

   procedure Parse_Heap (File : File_Type; Heap : in out Heap_Type) is
      Crate        : Crate_String;
      Stack_Number : Positive := 1;
      C            : Character;
   begin
   Crate_Reading_Loop:
      loop
         Get (File, Crate);
         if not End_Of_Line (File) then
            Get (File, C); -- skip space separator
         end if;

         case Crate (2) is
            when 'A' .. 'Z' =>
               -- crate symbol
               Add_Crate (Heap, Stack_Number, Crate (2));
            when ' ' =>
               -- empty space
               null;
            when '1' .. '9' =>
               -- stack number: indicate end of stacks
               Skip_Line (File); -- skip line with stack numbers
               Skip_Line (File); -- skip empty line
               exit Crate_Reading_Loop;
            when others =>
               raise Constraint_Error with "Invalid Crate: " & Crate;
         end case;

         Stack_Number := Stack_Number + 1;

         if End_Of_Line (File) then
            Skip_Line (File);
            Stack_Number := 1;
         end if;
      end loop Crate_Reading_Loop;
   end Parse_Heap;

   procedure Move_Crates1
     (Heap  : in out Heap_Type;
      Moves : in Moves_Type)
   is
      C : Character;
   begin
      for M of Moves loop
         for I in 1 .. M.Move loop
            C := Heap.Element (M.From).Last_Element;
            Heap.Reference (M.To).Append (C);
            Heap.Reference (M.From).Delete_Last;
         end loop;
      end loop;
   end Move_Crates1;

   procedure Move_Crates2
     (Heap  : in out Heap_Type;
      Moves : in Moves_Type) is
   begin
      for M of Moves loop
         for I in
           Heap(M.From).Last_Index - M.Move + 1 .. Heap(M.From).Last_Index
         loop
            Heap.Reference(M.To).Append (Heap.Element(M.From).Element(I));
         end loop;
         for I in 1 .. M.Move loop
            Heap.Reference(M.From).Delete_Last;
         end loop;
      end loop;
   end Move_Crates2;

   procedure Parse_Moves (File : File_Type; Moves : in out Moves_Type) is

      package Positive_IO is new Ada.Text_IO.Integer_IO (Num => Positive);
      use Positive_IO;

      Move      : Move_Type;
      Skip_Move : String := "move ";  -- buffer for skip move separator
      Skip_From : String := " from "; -- buffer for skip from separator
      Skip_To   : String := " to ";   -- buffer for skip to separator
   begin
      while not End_Of_Line (File) loop
         Get (File, Skip_Move);
         Get (File, Move.Move);
         Get (File, Skip_From);
         Get (File, Move.From);
         Get (File, Skip_To);
         Get (File, Move.To);
         Skip_Line (File);
         Moves.Append (Move);
      end loop;
   end Parse_Moves;

   procedure Top_Crates
     (Heap   : in Heap_Type;
      Crates : out Unbounded_String) is
   begin
      Crates := To_Unbounded_String ("");
      for Stack of Heap loop
         Append (Crates, Stack.Last_Element);
      end loop;
   end Top_Crates;

   procedure Part1 (Filename : String) is
      File   : File_Type;
      Heap   : Heap_Type;
      Moves  : Moves_Type;
      Result : Unbounded_String;
   begin
      Open (File, In_File, Filename);
      Parse_Heap (File, Heap);
      Parse_Moves (File, Moves);
      Close (File);

      Move_Crates1 (Heap, Moves);
      Top_Crates (Heap, Result);
      Put_Line ("Part1:");
      Put_Line ("  Crates on top: " & To_String (Result));
   end Part1;

   procedure Part2 (Filename : String) is
      File   : File_Type;
      Heap   : Heap_Type;
      Moves  : Moves_Type;
      Result : Unbounded_String;
   begin
      Open (File, In_File, Filename);
      Parse_Heap (File, Heap);
      Parse_Moves (File, Moves);
      Close (File);

      Move_Crates2 (Heap, Moves);
      Top_Crates (Heap, Result);
      Put_Line ("Part2:");
      Put_Line ("  Crates on top: " & To_String (Result));
   end Part2;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: day05 filename");
      Set_Exit_Status (Failure);
      return;
   end if;
   Part1 (Argument (1));
   Part2 (Argument (1));
end Day05;

-- ./bin/day05 input.txt 
-- Part1:
--   Crates on top: TLFGBZHCN
-- Part2:
--   Crates on top: QRQFHFWCL
