with Ada.Text_IO,
  Ada.Numerics.Float_Random;
use Ada.Text_IO,
  Ada.Numerics.Float_Random;


procedure bars is

   type Bar_Style is (Two, Swirl, White, Black);

   type Bar;
   type Bar_Ptr is access Bar;

   type Bar is tagged
      record
         L : Bar_Ptr;
         R : Bar_Ptr;
         Kind : Bar_Style;
         Pixels : Integer;
      end record;

   function MakeBar (Pix : Integer; Rnd : Generator) return Bar_Ptr is
      RetVal : Bar_Ptr;
   begin
      if 64.0 * Random(Rnd) ** 3 < Float(Pix) then
        if Random(Rnd) < 0.15 then --swirly style of mixing two bars
           RetVal := new Bar'(Kind => Swirl, Pixels => Pix,
                      L => MakeBar(Pix / 2, Rnd),
                      R => MakeBar(Pix / 2, Rnd));
        else
           RetVal := new Bar'(Kind => Two, Pixels => Pix,
                      L => MakeBar(Pix / 2, Rnd),
                      R => MakeBar(Pix / 2, Rnd));
        end if;
      else
         if Random(Rnd) < 0.5 then
            RetVal := new Bar'(Kind => Black, Pixels => Pix,
                               L => null, R => null);
         else
            RetVal := new Bar'(Kind => White, Pixels => Pix,
                               L => null, R => null);
         end if;
      end if;
      return RetVal;
   end MakeBar;

   function Emit(B : Bar_Ptr; X, Y: Integer) return Integer is
      Offset : Integer;
      Height : Integer := B.all.Pixels;
   begin
      case B.all.Kind is
         when Swirl =>
            Offset := Y+X mod Height;
            return Emit(B.all.R, X, Y) * Offset +
                   Emit(B.all.L, X, Y) * (1-Offset);
            --if ((Y+X) mod Height) * 2 > Height then
            --   return Emit(B.all.R, X, Y - Height);
            --else
            --   return Emit(B.all.L, X, Y);
            --end if;
         when Two =>
            if Y * 2 > Height then
               return Emit(B.all.R, X, Y - Height);
            else
               return Emit(B.all.L, X, Y);
            end if;
         when White =>
            return 0;
         when Black =>
            return 255;
      end case;
   end Emit;

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
         Put(Integer'Image(Emit(MainBar, X, Y)) & " ");
      end loop;
      Put_Line("");
   end loop;



   Put_Line(Integer'Image(Emit(MainBar, 0, 0)));

end;

