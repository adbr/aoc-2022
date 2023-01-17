-- adbr [2023-01-11 Wed]
-- Usage: day08 filename

with Ada.Text_IO;
with Ada.Command_Line;

procedure Day08 is
   
   use Ada.Text_IO;
   use Ada.Command_Line;
   
   subtype Height_Type is Natural range 0 .. 9;
   type Grid_Type is
     array (Positive range <>, Positive range <>) of Height_Type;
      
   ---------------
   -- Read_Data --
   ---------------
   
   function Read_Data (Filename : String) return Grid_Type is
      
      -------------------------
      -- Read_Grid_Dimension --
      -------------------------
      
      procedure Read_Grid_Dimension
        (Filename : in String;
         Rows     : out Positive;
         Cols     : out Positive)
      is
         File : File_Type;
         R    : Natural := 0;
         C    : Natural := 0;
      begin
         Open (File, In_File, Filename);
         while not End_Of_File (File) loop
            declare
               Line : constant String := Get_Line (File);
            begin
               R := R + 1;
               if C = 0 then
                  C := Line'Length;
               end if;
            end;
         end loop;
         Close (File);
         Rows := R;
         Cols := C;
      end Read_Grid_Dimension;
      
      --  Local variables
      
      Rows   : Positive;
      Cols   : Positive;
   begin
      Read_Grid_Dimension (Filename, Rows, Cols);
      declare
         Grid : Grid_Type (1 .. Rows, 1 .. Cols);
         File : File_Type;
         R    : Natural := 1;
         C    : Natural := 1;
         H    : String (1 .. 1);
      begin
         Open (File, In_File, Filename);
         while not End_Of_File (File) loop
            Get (File, H);
            Grid (R, C) := Height_Type'Value (H);
            if End_OF_Line (File) then
               R := R + 1;
               C := 1;
            else
               C := C + 1;
            end if;
         end loop;
         Close (File);
         return Grid;
      end;
   end Read_Data;
   
   ----------------
   -- Is_Visible --
   ----------------
   
   function Is_Visible
     (Grid : Grid_Type;
      Row  : Positive;
      Col  : Positive) return Boolean
   is
      
      -----------------------
      -- Is_Visible_In_Row --
      -----------------------
      
      function Is_Visible_In_Row
        (Grid : Grid_Type;
         Row  : Positive;
         Col  : Positive) return Boolean
      is
         Visible_Left  : Boolean := True;
         Visible_Right : Boolean := True;
      begin
         for C in Grid'First (2) .. Col - 1 loop
            if Grid (Row, C) >= Grid (Row, Col) then
               Visible_Left := False;
               exit;
            end if;
         end loop;
         
         for C in Col + 1 .. Grid'Last (2) loop
            if Grid (Row, C) >= Grid (Row, Col) then
               Visible_Right := False;
               exit;
            end if;
         end loop;
         
         return Visible_Left or Visible_Right;
      end Is_Visible_In_Row;

      -------------------------
      -- Is_Visible_In_Column --
      --------------------------
      
      function Is_Visible_In_Column
        (Grid : Grid_Type;
         Row  : Positive;
         Col  : Positive) return Boolean
      is
         Visible_Up   : Boolean := True;
         Visible_Down : Boolean := True;
      begin
         for R in Grid'First (1) .. Row - 1 loop
            if Grid (R, Col) >= Grid (Row, Col) then
               Visible_Up := False;
               exit;
            end if;
         end loop;
         
         for R in Row + 1 .. Grid'Last (1) loop
            if Grid (R, Col) >= Grid (Row, Col) then
               Visible_Down := False;
               exit;
            end if;
         end loop;
         
         return Visible_Up or Visible_Down;
      end Is_Visible_In_Column;

   begin
      return Is_Visible_In_Row (Grid, Row, Col) or
        Is_Visible_In_Column (Grid, Row, Col);
   end Is_Visible;
   
   -----------
   -- Part1 --
   -----------
   
   procedure Part1 (Grid : in Grid_Type) is
      Visible : Natural := 0;
   begin
      for R in Grid'Range (1) loop
         for C in Grid'Range (2) loop
            if Is_Visible (Grid, R, C) then
               Visible := Visible + 1;
            end if;
         end loop;
      end loop;
      Put_Line ("Part 1: number of visible trees:" & Visible'Image);
   end Part1;

   ---------------------------
   -- Viewing_Distance_Left --
   ---------------------------
   
   function Viewing_Distance_Left
     (Grid : Grid_Type;
      Row  : Positive;
      Col  : Positive) return Natural
   is
      Distance : Natural := 0;
   begin
      for C in reverse Grid'First (2) .. Col - 1 loop
         Distance := Distance + 1;
         if Grid (Row, C) >= Grid (Row, Col) then
            exit;
         end if;
      end loop;
      return Distance;
   end Viewing_Distance_Left;

   ---------------------------
   -- Viewing_Distance_Right --
   ---------------------------
   
   function Viewing_Distance_Right
     (Grid : Grid_Type;
      Row  : Positive;
      Col  : Positive) return Natural
   is
      Distance : Natural := 0;
   begin
      for C in Col + 1 .. Grid'Last (2) loop
         Distance := Distance + 1;
         if Grid (Row, C) >= Grid (Row, Col) then
            exit;
         end if;
      end loop;
      return Distance;
   end Viewing_Distance_Right;

   ---------------------------
   -- Viewing_Distance_Up --
   ---------------------------
   
   function Viewing_Distance_Up
     (Grid : Grid_Type;
      Row  : Positive;
      Col  : Positive) return Natural
   is
      Distance : Natural := 0;
   begin
      for R in reverse Grid'First (1) .. Row - 1 loop
         Distance := Distance + 1;
         if Grid (R, Col) >= Grid (Row, Col) then
            exit;
         end if;
      end loop;
      return Distance;
   end Viewing_Distance_Up;

   ---------------------------
   -- Viewing_Distance_Down --
   ---------------------------
   
   function Viewing_Distance_Down
     (Grid : Grid_Type;
      Row  : Positive;
      Col  : Positive) return Natural
   is
      Distance : Natural := 0;
   begin
      for R in Row + 1 .. Grid'Last (1) loop
         Distance := Distance + 1;
         if Grid (R, Col) >= Grid (Row, Col) then
            exit;
         end if;
      end loop;
      return Distance;
   end Viewing_Distance_Down;

   -----------
   -- Part2 --
   -----------
   
   procedure Part2 (Grid : in Grid_Type) is
      Max_Scenic_Score : Natural := 0;
   begin
      for R in Grid'Range (1) loop
         for C in Grid'Range (2) loop
            declare
               Scenic_Score : Natural;
            begin
               Scenic_Score := Viewing_Distance_Left (Grid, R, C) *
                 Viewing_Distance_Right (Grid, R, C) *
                 Viewing_Distance_Up (Grid, R, C) *
                 Viewing_Distance_Down (Grid, R, C);
               if Scenic_Score > Max_Scenic_Score then
                  Max_Scenic_Score := Scenic_Score;
               end if;
            end;
         end loop;
      end loop;
      Put_Line ("Part 2: highest scenic score:" & Max_Scenic_Score'Image);
   end Part2;
   
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: day08 filename");
      Set_Exit_Status (Failure);
      return;
   end if;
   
   declare
      Grid : constant Grid_Type := Read_Data (Argument (1));
   begin
      Part1 (Grid);
      Part2 (Grid);
   end;
end Day08;

-- ./bin/day08 input.txt 
-- Part 1: number of visible trees: 1715
-- Part 2: highest scenic score: 374400
