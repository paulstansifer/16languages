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

   package Nodes is

   end Nodes;

   package body Nodes is
   end Nodes;

   use Nodes;

   --Light_Source <: Object ?
   --Transformation <: Warp ?

   type POVType is (Float,
                    Object,
                    Material,
                    Light_Source,
                    Transformation,
                    Warp,
                    --String,
                    Coord, --For POV-Ray, a vector/color
                    Tuple,
                    List,
                    Func
                    );

   type MaterialName is (Pigment, Finish, Normal, Interior);

   package POVRay is
      --type PType;
      --type PTP is access PType'Class;

      type PType is tagged record
        Value : POVType;
      end record;
      type PTP is access PType'Class;
      type PTP_Array is array (Integer) of PTP;


      type Node;
      type Node_Ptr is access Node'Class;
      type Node_Array is array (Integer) of Node_Ptr;
      type NodeType is (Func_Call, Literal, Coord, Tuple, List);
      type Node is tagged
         record
            Contents : Standard.String(Integer);
            Children : Node_Array;
            Determined_Type : PTP;
            NType : NodeType;
         end record;

      function Matches(T : PType ; N : Node'Class) return Boolean;

      type PFuncType is new PType with record
         Args : PTP_Array;
      end record;
      function Matches(T : PFuncType ; N : Node'Class) return Boolean;

      type PCoordType is new PType with record
         Size : Integer;
      end record;
      function Matches(T : PCoordType ; N : Node'Class) return Boolean;

      type PTupType is new PType with record
         Elems : PTP_Array;
      end record;
      function Matches(T : PTupType ; N : Node'Class) return Boolean;

      procedure Expect(T : PTP ; N : access Node'Class);

      function NodeInp return Node_Ptr;
      procedure Scene(Elements : Node_Array);
      function Nd(Kind : NodeType ; Text : Standard.String ; Kids : Node_Array) return Node_Ptr;

      Object_Type : PTP := new PType'(Value => Object);

   end POVRay;

   use Ada.Strings.Unbounded;

   package body POVRay is

      function GetToken return String is
         Buffer : Unbounded_String := Null_Unbounded_String;
      begin
         return "asdf";
         --Ada.Text_IO.Get(CChar);
      end GetToken;

      function NodeInp return Node_Ptr is
      begin
         return null;
      end NodeInp;

      procedure Scene(Elements : Node_Array) is
      begin
         for I in Elements'Range loop
            Expect(Object_Type, null);--Elements(I));
         end loop;
      end Scene;

      function Nd(Kind : NodeType ; Text : Standard.String ; Kids : Node_Array) return Node_Ptr is
      begin
         --return new Node'(NType => Kind, Contents => Text, Children => Kids, Determined_Type => null);
         return new Node'(NType => Kind , Contents => Text , Children => Kids , Determined_Type => null);
      end Nd;

      function Matches(T : PType ; N : Node'Class) return Boolean is
      begin
         return False;
      end Matches;

      function Matches(T : PFuncType ; N : Node'Class) return Boolean is
      begin
         if N.NType = Func_Call then
            return False;
         end if;

         return True;
      end Matches;

      function Matches(T : PCoordType ; N : Node'Class) return Boolean is
      begin
         if N.NType = Coord then
            return False;
         end if;

         return True;
      end Matches;

      function Matches(T : PTupType ; N : Node'Class) return Boolean is
      begin
         if N.NType = Tuple then
            return False;
         end if;

         return True;
      end Matches;

      procedure Expect(T : PTP ; N : access Node'Class) is
      begin
         if Matches(T.all, N.all) then
            N.Determined_Type := T;
         end if;
      end Expect;

   end POVRay;

   use POVRay;
begin


   Ada.Text_IO.Put("hello, world!");
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
