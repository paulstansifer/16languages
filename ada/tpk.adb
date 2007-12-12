with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure tpk is

   package FMath is new Ada.Numerics.Generic_Elementary_Functions(Float);
   package F_IO is new Ada.Text_IO.Float_IO(Float);

   function F(X : Float) return Float is
   begin
      return FMath.Sqrt(abs(X)) + 5.0 * X ** 3;
   end F;

   A : array (Integer range 0 .. 10) of Float;
   Res : Float;
begin

   for I in A'Range loop
      F_IO.Get(A(I));
   end loop;
   for I in reverse 0 .. 10 loop
      Res := F(A(I));
      Ada.Text_IO.Put(Integer'Image(I));
      if Res > 400.0 then
         Ada.Text_IO.Put_Line(" TOO LARGE");
      else
         Ada.Text_IO.Put_Line(Float'Image(Res));
      end if;
   end loop;
end;
