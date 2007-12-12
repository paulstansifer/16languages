with Ada.Text_IO, Ada.Strings.Unbounded;
use  Ada.Text_IO;

--
-- Scene( N(Func_Call, "union",
--  (N(Func_Call, "cube",
--    (N(Coord, "",
--      (N(Literal, "0.0", ()), N(Literal, "0.0", ()), N(Literal, "0.0", ()))),
--    N(Coord, "",
--      (N(Literal, "1.0", ()), N(Literal, "1.0", ()), N(Literal, "1.0", ()))))
--  )
--))

procedure Static is

   package Str renames Ada.Strings.Unbounded;


   use Ada.Strings.Unbounded;
   --Light_Source <: Object ?
   --Transformation <: Warp ?

   type POVType is (Float,
                    Object,
                    Material,
                    Light_Source,
                    Transformation,
                    Warp,
                    Char--,
                    --Coord, --For POV-Ray, a vector/color
                    --Tuple,
                    --List,
                    --Func
                    );

   type MaterialName is (Pigment, Finish, Normal, Interior);



   package POVRay is
      --type PType;
      --type PTP is access PType'Class;


      type PType;
      type PTP is access PType'Class;
      --type PTP_Array is array (Integer) of PTP;

      type Node;
      type Node_Ptr is access Node'Class;

      type NodeListImp;
      type NodeList is access NodeListImp'Class;
      type NodeListImp is tagged
         record
            Elem : Node_Ptr;
            Next : NodeList;
         end record;

      type Node_Array is array (Integer) of Node_Ptr;
      type NodeType is (Func_Call, IntLiteral, StringLiteral, Coord, Tuple, List);
      type Node is tagged
         record
            Contents : Unbounded_String;
            Children : NodeList;
            Determined_Type : PTP;
            NType : NodeType;
         end record;


--      function Matches(T : PType ; N : Node'Class) return Boolean;

      type TypeListImp;
      type TypeList is access TypeListImp'Class;
      type TypeListImp is tagged
         record
            Elem : PTP;
            Next : TypeList;
         end record;

      ------------

      type TypeClass is (Plain, Func, Coord, Tuple, List);
      type PType is tagged record
         Category : TypeClass;
         Primary : POVType;
         Composite : TypeList;
         Size : Integer;
      end record;


   end POVRay;

   package body POVRay is
      function Get_Token return String is
      begin
         return Ada.Text_IO.Get_Line;
      end Get_Token;

      function Node_Inp return Node_Ptr is
         Kind : NodeType;
         Contents : Unbounded_String;
         Kids_First : NodeList := null;
         Kids_Last : NodeList := null;
      begin
         Kind := NodeType'Value(Get_Token);
         Contents := To_Unbounded_String(Get_Token);
         while Get_Token = "(" loop
            if Kids_Last = null then
               Kids_Last := new NodeListImp'(Elem => Node_Inp, Next => null);
               Kids_First := Kids_Last;
            else
               Kids_Last.all.Next := new NodeListImp'(Elem => Node_Inp, Next => null);
            end if;
         end loop;

         return Nd(Kind, Contents, Kids_First);
      end Node_Inp;



      function Nd(Kind : NodeType ; Text : Unbounded_String ; Kids : NodeList) return Node_Ptr is
      begin
         return new Node'(NType => Kind, Contents => Text, Children => Kids, Determined_Type => null);
      end Nd;




   end POVRay;

   use POVRay;
begin
   GetToken; --ignore the first '('
   Scene( Node_Inp );
   --Scene( (N(Func_Call, "union",
   --         (N(Func_Call, "cube",
   --            (N(Coord, "",
   --               (N(Literal, "0.0", None), N(Literal, "0.0", None), N(Literal, "0.0", None))),
   --             N(Coord, "",
   --               (N(Literal, "1.0", None), N(Literal, "1.0", None), N(Literal, "1.0", None))))
   --            ),
   --          N(Func_Call, "sphere",
   --            (N(Coord, "",
   --               (N(Literal, "0.0", None), N(Literal, "0.0", None), N(Literal, "0.0", None))),
   --             N(Literal, "1.0", None))
   --            )
   --          )),
   --        N(Func_Call, "light_source", None)
   --       ));
end Static;
