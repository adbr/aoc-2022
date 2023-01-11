-- adbr [2022-12-07 Wed]

with Ada.Text_IO;
with Ada.Command_Line;

procedure Day04 is
   use Ada.Text_IO;
   use Ada.Command_Line;

   subtype Section_Type is Positive;

   type Range_Type is record
      First : Section_Type;
      Last  : Section_Type;
   end record;

   type Pair_Type is record
      Range1 : Range_Type;
      Range2 : Range_Type;
   end record;

   package Section_IO is new Ada.Text_IO.Integer_IO (Num => Section_Type);
   use Section_IO;

   function Is_Full_Overlap (Pair : Pair_Type) return Boolean is
   begin
      if (Pair.Range1.First >= Pair.Range2.First and
            Pair.Range1.Last <= Pair.Range2.Last) or
        (Pair.Range2.First >= Pair.Range1.First and
           Pair.Range2.Last <= Pair.Range1.last)
      then
         return True;
      else
         return False;
      end if;
   end Is_Full_Overlap;

   function Is_Overlap (Pair : Pair_Type) return Boolean is
   begin
      if Pair.Range1.First >= Pair.Range2.First and
        Pair.Range1.First <= Pair.Range2.Last then
         return True;
      elsif Pair.Range1.Last >= Pair.Range2.First and
        Pair.Range1.Last <= Pair.Range2.Last then
         return True;
      elsif Pair.Range2.First >= Pair.Range1.First and
        Pair.Range2.First <= Pair.Range1.Last then
         return True;
      elsif Pair.Range2.Last >= Pair.Range1.First and
        Pair.Range2.Last <= Pair.Range1.Last then
         return True;
      else
         return False;
      end if;
   end Is_Overlap;

   procedure Part1 (Filename : String) is
      File          : File_Type;
      Pair          : Pair_Type;
      Sep           : Character; -- for skip character
      Overlaps      : Natural := 0;
      Full_Overlaps : Natural := 0;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop

         -- get range 1
         Get (File, Pair.Range1.First);
         Get (File, Sep); -- skip '-'
         Get (File, Pair.Range1.Last);
         Get (File, Sep); -- skip ','

         -- get range 2
         Get (File, Pair.Range2.First);
         Get (File, Sep); -- skip '-'
         Get (File, Pair.Range2.Last);

         if Is_Full_Overlap (Pair) then
            Full_Overlaps := Full_Overlaps + 1;
         end if;
         if Is_Overlap (Pair) then
            Overlaps := Overlaps + 1;
         end if;
      end loop;
      Close (File);
      Put_Line ("Part 1:");
      Put_Line ("  Number of full overlap pairs:" & Full_Overlaps'Image);
      Put_Line ("Part 2:");
      Put_Line ("  Number of overlap pairs:" & Overlaps'Image);
   end Part1;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: day04 filename");
      Set_Exit_Status (Failure);
      return;
   end if;

   Part1 (Argument (1));
end Day04;

-- ./bin/day04 input.txt
-- Part 1:
--   Number of full overlap pairs: 485
-- Part 2:
--   Number of overlap pairs: 857
