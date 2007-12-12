with Ada.Text_IO,
  Ada.Numerics.Float_Random;
use Ada.Text_IO,
  Ada.Numerics.Float_Random;


procedure bars2 is
   package Bars is
      type Bar;
      type Bar_Ptr is access Bar'Class;

      type Bar is abstract tagged
         record
            Pixels : Integer;
         end record;

      function Emit(B : Bar ; X, Y : Integer) return Integer is abstract;
   end Bars;

   use Bars;

   package Pairs is
      type Pair is new Bar with record
         L, R : Bar_Ptr;
      end record;
      function Emit(B : Pair ; X, Y : Integer) return Integer;
   end Pairs;

   package Colors is
      type Color is new Bar with record
         Value : Integer;
      end record;
      function Emit(B : Color ; X, Y : Integer) return Integer;
   end Colors;

   package Dupes is
      type Dupe is new Bar with record
         Child : Bar_Ptr;
      end record;
      function Emit(B : Dupe ; X, Y : Integer) return Integer;
   end Dupes;

   package Swirls is
      type Swirl is new Pairs.Pair with null record;
      function Emit(B : Swirl ; X, Y : Integer) return Integer;
   end Swirls;

   use Colors, Dupes, Pairs, Swirls;

   package body Colors is
      function Emit(B : Color; X, Y : Integer) return Integer is
      begin
         return B.Value;
      end Emit;
   end Colors;

   package body Pairs is
      function Emit(B : Pair; X, Y : Integer) return Integer is
         L, R : Bar_Ptr;
      begin
         L := B.L;
         R := B.R;
         if Y > B.Pixels / 2 then
            return Bars.Emit(R.all, X, Y - B.Pixels / 2);
         else
            return Emit(L.all, X, Y);
         end if;
      end Emit;
   end Pairs;

   package body Dupes is
      function Emit(B : Dupe; X, Y : Integer) return Integer is
      begin
         return Emit(B.Child.all, X, Y mod (B.Pixels / 2));
      end Emit;
   end Dupes;

   package body Swirls is
      function Emit(B : Swirl; X, Y: Integer) return Integer is
         Offset : Integer;
         Height : Integer := B.Pixels;
      begin
         Offset := (Y+X) mod Height;
         --return Emit(B.R.all, X, Y) * Offset +
         --  Emit(B.L.all, X, Y) * (1-Offset);
         if Offset * 2 > Height then
            return Emit(B.R.all, X, Y mod Height); --Offset );
         else
            return Emit(B.L.all, X, Y mod Height);
         end if;
      end Emit;
   end Swirls;

      function MakeBar (Pix : Integer; Rnd : Generator) return Bar_Ptr is
      RetVal : Bar_Ptr;
   begin
      if 64.0 * Random(Rnd) ** 3 < Float(Pix) then
        if Random(Rnd) < 0.15 then --swirly style of mixing two bars
           RetVal := New Swirl'(Pixels => Pix,
                                L => MakeBar(Pix / 2, Rnd),
                                R => MakeBar(Pix / 2, Rnd));
        elsif Random(Rnd) < 0.2 then
           RetVal := new Dupe'(Pixels => Pix,
                               Child => MakeBar(Pix / 2, Rnd));
        else
           RetVal := new Pair'(Pixels => Pix,
                      L => MakeBar(Pix / 2, Rnd),
                      R => MakeBar(Pix / 2, Rnd));
        end if;
      else
         RetVal := new Color'(Pixels => Pix,
                      Value => Integer(Random(Rnd)*255.0));
      end if;
      return RetVal;
   end MakeBar;


   X_Size, Y_Size : Integer;
   MainBar : Bar_Ptr;
   Rnd : Generator;
begin
   Reset(Rnd);
   X_Size := 256;
   Y_Size := 512;

   MainBar := MakeBar(Y_Size, Rnd);

   Put_Line("P2");
   Put_Line(Integer'Image(X_Size) & " " & Integer'Image(Y_Size));
   Put_Line("255");

   for Y in Integer range 0 .. Y_Size-1 loop
      for X in Integer range 0 .. X_Size-1 loop
         Put(Integer'Image(Emit(MainBar.all, X, Y)) & " ");
      end loop;
      Put_Line("");
   end loop;



   Put_Line(Integer'Image(Emit(MainBar.all, 0, 0)));

end;

