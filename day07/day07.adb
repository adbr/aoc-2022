-- adbr [2022-12-28 Wed]
-- Usage: day07 filename

with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

procedure Day07 is
   use Ada.Text_IO;
   use Ada.Text_IO.Unbounded_IO;
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;

   Input_Error : exception;
   Data_Error  : exception;

   type Command_Type is
      record
         Name     : Unbounded_String;
         Argument : Unbounded_String;
      end record;

   type Entry_Kind_Type is (File_Entry, Directory_Entry);

   type Entry_Type;
   type Entry_Access_Type is access Entry_Type;

   package Entry_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Entry_Access_Type);
   subtype Entry_Vector is Entry_Vectors.Vector;

   type Entry_Type (Kind : Entry_Kind_Type) is
      record
         Name : Unbounded_String;
         case Kind is
            when File_Entry =>
               Size : Natural;
            when Directory_Entry =>
               Parent : Entry_Access_Type;
               Childs : Entry_Vector;
         end case;
      end record;
   
   ----------------------
   -- Entry_Deallocate --
   ----------------------
   
   procedure Entry_Deallocate is new Ada.Unchecked_Deallocation
     (Object => Entry_Type, Name => Entry_Access_Type);
   
   -------------------
   -- Dir_Dealocate --
   -------------------
   
   procedure Dir_Dealocate (Dir : in out Entry_Access_Type) is
   begin
      for E of Dir.Childs loop
         case E.Kind is
            when File_Entry =>
               Entry_Deallocate (E);
            when Directory_Entry =>
               Dir_Dealocate (E);
         end case;
      end loop;
      Entry_Deallocate (Dir);
   end Dir_Dealocate;

   -------------------
   -- Parse_Command --
   -------------------

   procedure Parse_Command
     (Line    : in Unbounded_String;
      Command : out Command_Type)
   is
      Name : Unbounded_String;
   begin
      if Element (Line, 1) /= '$' then
         raise Input_Error
           with "Wrong command line: '" & To_String (Line) & "'";
      end if;

      Name := Unbounded_Slice (Line, 3, 4);
      if Name /= "cd" and Name /= "ls" then
         raise Input_Error
           with "Wrong command: '" & To_String (Name) & "'";
      end if;

      Command.Name := Name;
      if Name = "cd" then
         Command.Argument := Unbounded_Slice (Line, 6, Length (Line));
      else
         Command.Argument := To_Unbounded_String ("");
      end if;
   end Parse_Command;

   -------------------
   -- Print_Command --
   -------------------

   procedure Print_Command (Command : Command_Type) is
   begin
      Put_Line ("Command: [Name:" & Command.Name & ", " &
                  "Argument:" & Command.Argument & "]");
   end Print_Command;

   -----------------
   -- Parse_Entry --
   -----------------

   procedure Parse_Entry
     (Line         : in Unbounded_String;
      Entry_Access : out Entry_Access_Type)
   is
      C : constant Character := Element (Line, 1);
   begin
      case C is
         when 'd' =>
            -- directory
            Entry_Access := new Entry_Type (Kind => Directory_Entry);
            declare
               I : constant Natural := Index (Line, " ");
            begin
               Entry_Access.Name := Unbounded_Slice (Line, I+1, Length (Line));
               Entry_Access.Childs := Entry_Vectors.To_Vector (0);
            end;
         when '0' .. '9' =>
            -- file
            Entry_Access := new Entry_Type (Kind => File_Entry);
            declare
               I : constant Natural := Index (Line, " ");
            begin
               Entry_Access.Name := Unbounded_Slice (Line, I+1, Length (Line));
               Entry_Access.Size := Natural'Value (Slice (Line, 1, I-1));
            end;
         when others =>
            raise Input_Error with "Wrong dir entry: " & To_String (Line);
      end case;
   end Parse_Entry;

   -----------------
   -- Print_Entry --
   -----------------

   procedure Print_Entry
     (Entry_Access : in Entry_Access_Type;
      Indent       : in String)
   is
      Indent_Step : constant String := "  ";
   begin
      Put (Indent);
      case Entry_Access.Kind is
         when File_Entry =>
            Put_Line ("Entry: [Kind:" & Entry_Access.Kind'Image & ", " &
                        "Name:" & Entry_Access.Name & ", " &
                        "Size:" & Entry_Access.Size'Image & "]");
         when Directory_Entry =>
            Put_Line ("Entry: [Kind:" & Entry_Access.Kind'Image & ", " &
                        "Name:" & Entry_Access.Name & "]");
            for E of Entry_Access.Childs loop
               Print_Entry (E, Indent & Indent_Step);
            end loop;
      end case;
      -- Put_Line (Entry_Access.all'Image); --FIXME: sprawdzić (-gnat2020)
   end Print_Entry;

   -----------------
   -- Parse_Input --
   -----------------

   procedure Parse_Input
     (Filename : in String;
      Root     : in out Entry_Access_Type)
   is

      --------------------
      -- Find_Directory --
      --------------------

      function Find_Directory
        (Dir  : in Entry_Access_Type;
         Name : in Unbounded_String) return Entry_Access_Type
      is
      begin
         for E of Dir.Childs loop
            if E.Kind = Directory_Entry and E.Name = Name then
               return E;
            end if;
         end loop;
         raise Data_Error with "Missing directory: " & To_String (Name);
      end Find_Directory;

      --  Local variables

      File : File_Type;
      Line : Unbounded_String;
      Dir  : Entry_Access_Type := null; -- current directory

   --  Start of processing for Parse_Input

   begin
      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         Get_Line (File, Line);
         if Element (Line, 1) = '$' then
            -- command: cd or ls
            declare
               Cmd : Command_Type;
            begin
               Parse_Command (Line, Cmd);
               if Cmd.Name = "cd" then
                  if Cmd.Argument = "/" then
                     Root        := new Entry_Type (Kind => Directory_Entry);
                     Root.Name   := Cmd.Argument;
                     Root.Parent := null;
                     Root.Childs := Entry_Vectors.To_Vector (0);
                     Dir         := Root;
                  elsif Cmd.Argument = ".." then
                     Dir := Dir.Parent;
                  else
                     Dir := Find_Directory (Dir, Cmd.Argument);
                  end if;
               elsif Cmd.Name = "ls" then
                  null;
               end if;
            end;
         else
            -- entry: directory or file
            declare
               Ent : Entry_Access_Type;
            begin
               Parse_Entry (Line, Ent);
               if Ent.Kind = Directory_Entry then
                  Ent.Parent := Dir;
               end if;
               Dir.Childs.Append (Ent);
            end;
         end if;
      end loop;
      Close (File);
   end Parse_Input;

   --------------
   -- Dir_Size --
   --------------

   function Dir_Size (Dir : in Entry_Access_Type) return Natural is
      Result : Natural := 0;
   begin
      for E of Dir.Childs loop
         case E.Kind is
            when File_Entry =>
               Result := Result + E.Size;
            when Directory_Entry =>
               Result := Result + Dir_Size (E);
         end case;
      end loop;
      return Result;
   end Dir_Size;

   -----------
   -- Part1 --
   -----------

   procedure Part1 (Root : Entry_Access_Type) is
      
      -------------------------
      -- Search_Smaller_Dirs --
      -------------------------

      procedure Search_Smaller_Dirs
        (Dir          : in Entry_Access_Type;
         Max_Dir_Size : in Natural;
         Total_Size   : in out Natural)
      is
         Size : Natural := 0;
      begin
         for E of Dir.Childs loop
            if E.Kind = Directory_Entry then
               Search_Smaller_Dirs (E, Max_Dir_Size, Total_Size);
            end if;
         end loop;
         
         Size := Dir_Size (Dir);
         if Size <= Max_Dir_Size then
            Total_Size := Total_Size + Size;
         end if;
      end Search_Smaller_Dirs;
      
      --  Local variables
      
      Max_Dir_Size : constant Natural := 100000;
      Total_Size   : Natural          := 0;
      
   begin
      Search_Smaller_Dirs (Root, Max_Dir_Size, Total_Size);
      Put_Line ("Part 1: sum of smaller directory sizes:" & Total_Size'Image);
   end Part1;

   -----------
   -- Part2 --
   -----------
   
   procedure Part2 (Root : Entry_Access_Type) is
   
      -----------------------
      -- Search_Delete_Dir --
      -----------------------
      
      procedure Search_Delete_Dir
        (Dir             : in Entry_Access_Type;
         Min_Delete_Size : in Natural;
         Delete_Size     : in out Natural)
      is
         Size : Natural := 0;
      begin
         for E of Dir.Childs loop
            if E.Kind = Directory_Entry then
               Search_Delete_Dir (E, Min_Delete_Size, Delete_Size);
            end if;
         end loop;
         
         Size := Dir_Size (Dir);
         if Size >= Min_Delete_Size then
            if Delete_Size = 0 then
               Delete_Size := Size;
            end if;
            if Size < Delete_Size then
               Delete_Size := Size;
            end if;
         end if;
      end Search_Delete_Dir;
      
      --  Local variables
      
      Total_Space     : constant Natural := 70_000_000;
      Required_Space  : constant Natural := 30_000_000;
      Total_Size      : constant Natural := Dir_Size (Root);
      Min_Delete_Size : constant Natural :=
        Required_Space - (Total_Space - Total_Size);
      Delete_Size     : Natural          := 0;
      
   begin
      Search_Delete_Dir (Root, Min_Delete_Size, Delete_Size);
      Put_Line ("Part 2: smalest delete drectory:" & Delete_Size'Image);
   end Part2;
   
--  Start of processing for Day07

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: day07 filename");
      Set_Exit_Status (Failure);
      return;
   end if;
   
   declare
      Root : Entry_Access_Type;
   begin
      Parse_Input (Argument (1), Root);
      Part1 (Root);
      Part2 (Root);
      Dir_Dealocate (Root);
   end;
end Day07;

-- ./bin/day07 input.txt 
-- Part 1: sum of smaller directory sizes: 1348005
-- Part 2: smalest delete drectory: 12785886
