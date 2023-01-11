--  adbr [2022-12-02 Fri]
--
--  Usage: day02 filename

with Ada.Text_IO;
with Ada.Command_Line;

procedure Day02 is
   use Ada.Text_IO;
   use Ada.Command_Line;

   Value_Error : exception;

   type Shape_Type is (Rock, Paper, Scissors);
   type Round_Type is record
      Player1 : Shape_Type; -- opponent
      Player2 : Shape_Type; -- self
   end record;
   type Outcome_Type is (Lose, Draw, Win); -- outcome for self

   function Round (Player1_Code, Player2_Code : Character) return Round_Type is
      Result : Round_Type;

      function Shape (Code : Character) return Shape_Type is
      begin
         case Code is
            when 'A' | 'X' => return Rock;
            when 'B' | 'Y' => return Paper;
            when 'C' | 'Z' => return Scissors;
            when others =>
               raise Value_Error;
         end case;
      end Shape;

   begin
      Result.Player1 := Shape (Player1_Code);
      Result.Player2 := Shape (Player2_Code);
      return Result;
   end Round;

   function Round2 (Player1_Code, Player2_Code : Character) return Round_Type is
      Result : Round_Type;

      function Opponent_Shape (Code : Character) return Shape_Type is
         Result : Shape_Type;
      begin
         case Code is
            when 'A' => Result := Rock;
            when 'B' => Result := Paper;
            when 'C' => Result := Scissors;
            when others =>
               raise Value_Error;
         end case;
         return Result;
      end Opponent_Shape;
      
      function Outcome (Code : Character) return Outcome_Type is
         Result : Outcome_Type;
      begin
         case Code is
            when 'X' => Result := Lose;
            when 'Y' => Result := Draw;
            when 'Z' => Result := Win;
            when others =>
               raise Value_Error;
         end case;
         return Result;
      end Outcome;
      
      function Self_Shape (Code1, Code2 : Character) return Shape_Type is
         Self_Shapes : array (Shape_Type, Outcome_Type) of Shape_Type :=
           (Rock     => (Draw => Rock,
                         Win  => Paper,
                         Lose => Scissors),
            Paper    => (Lose => Rock,
                         Draw => Paper,
                         Win  => Scissors),
            Scissors => (Win  => Rock,
                         Lose => Paper,
                         Draw => Scissors));
      begin
         return  Self_Shapes (Opponent_Shape (Code1), Outcome (Code2));
      end Self_Shape;
      
   begin
      Result.Player1 := Opponent_Shape (Player1_Code);
      Result.Player2 := Self_Shape (Player1_Code, Player2_Code);
      return Result;
   end Round2;
   
   function Outcome (Round : Round_Type) return Outcome_Type is
      -- First shape in round: oponent
      -- Second shape in round: self
      -- Outcome is for self
      Outcomes : array (Shape_Type, Shape_Type) of Outcome_Type :=
        (Rock     => (Rock     => Draw,
                      Paper    => Win,
                      Scissors => Lose),
         Paper    => (Rock     => Lose,
                      Paper    => Draw,
                      Scissors => Win),
         Scissors => (Rock     => Win,
                      Paper    => Lose,
                      Scissors => Draw));
   begin
      return Outcomes (Round.Player1, Round.Player2);
   end Outcome;

   function Score (Round : Round_Type) return Natural is
      Result : Natural;
      Shape_Score : array (Shape_Type) of Natural :=
        (Rock => 1, Paper => 2, Scissors => 3);
      Outcome_Score : array (Outcome_Type) of Natural :=
        (Lose => 0, Draw => 3, Win => 6);
   begin
      Result := Shape_Score (Round.Player2);
      Result := Result + Outcome_Score (Outcome (Round));
      return Result;
   end Score;

   procedure Part1 (Filename : String) is
      File        : File_Type;
      C1, C2      : Character;
      Space       : Character; -- for skip space
      Total_Score : Natural := 0;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Get (File, C1);
         Get (File, Space);
         Get (File, C2);
         Total_Score := Total_Score + Score (Round (C1, C2));
      end loop;
      Close (File);
      Put_Line ("Part 1:");
      Put_Line ("  Total score: " & Total_Score'Image);
   end Part1;

   procedure Part2 (Filename : String) is
      File        : File_Type;
      C1, C2      : Character;
      Space       : Character; -- for skip space
      Total_Score : Natural := 0;
   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Get (File, C1);
         Get (File, Space);
         Get (File, C2);
         Total_Score := Total_Score + Score (Round2 (C1, C2));
      end loop;
      Close (File);
      Put_Line ("Part 2:");
      Put_Line ("  Total score: " & Total_Score'Image);
   end Part2;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: day02 filename");
      Set_Exit_Status (Failure);
      return;
   end if;

   Part1 (Argument (1));
   Part2 (Argument (1));
end Day02;

-- ./bin/day02 input.txt 
-- Part 1:
--   Total score:  11767
-- Part 2:
--   Total score:  13886
