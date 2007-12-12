with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Exceptions;
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

    type Prim_Type is (Float,
                      Object,
                      Material,
                      Light_Source,
                      Transformation,
                      Warp,
                      Char,
                      Action, --type for side-effecting code
                      Not_Applicable);
   

    type MaterialName is (Pigment, Finish, Normal, Interior);
   
    package POVRay is
        type PType;
        type PTP is access PType;

        type Node;
        type Node_Ptr is access Node;--'Class;

        type Node_ListImp;
        type Node_List is access Node_ListImp;--'Class;
        type Node_ListImp is tagged
            record
                Elem : Node_Ptr;
                Next : Node_List;
            end record;

        --names can be introduced by Lambda, Def, MDef, and Proc_Def
        --scopes can be introduced  Lambda, Block, and Proc_Def
        type Node_Type is (X, --for making trees of 
                          Func_Call, -- fn (expr) args (tuple)
                          Float_Literal, 
                          Lambda, -- args (names) val (expr)
                          Coord, Tuple, List, -- different composite types
                          Def_Var, -- name (name) val (expr)
                          Def_MVar, -- name (name) val (expr)
                          Block, -- action (proc) next (block)
                          Def_Proc, -- name (name) largs (names) rargs (names) body (block)
                          Proc, -- name (name) largs (names) rargs (exprs)
                          Var_Ref, MVar_Ref, P_Call); -- name
        type Node is tagged
            record
                Contents : Unbounded_String;
                Children : Node_List;
                Determined_Type : aliased PTP;
                NType : Node_Type;
                Referrent : Node_Ptr;
                --Line : Integer; --TODO: let people know where errors occur
            end record;

        procedure Append(List : Node_List ; Elem : Node_Ptr);

    type Var_Def;
    type Var_Def_Ptr is access Var_Def;
    type Var_Def is tagged record
        Name : Unbounded_String;
        Def : Node_Ptr;
        Namespace : Node_Type;
        Next : Var_Def_Ptr;
    end record;

    type Scope;
    type Scope_Ptr is access Scope;
    type Scope is tagged record
         Up : Scope_Ptr;
         First : Var_Def_Ptr;
    end record;

    Top_Scope : Scope_Ptr := new Scope'(Up => null, First => null);

    function Find(Name : Unbounded_String; Namespace : Node_Type) return Node_Ptr;
    function In_Top_Scope(Name : Unbounded_String; Namespace : Node_Type) return Boolean;
    procedure Define(Name : Unbounded_String ; Def : Node_Ptr);


       ------------------

    type TypeListImp;
    type TypeList is access TypeListImp'Class;
    type TypeListImp is tagged
    record
        Elem : PTP;
        Next : TypeList;
    end record;

      type TypeClass is (Plain, Func, Coord, Tuple, List);
      type PType is record
         Category : TypeClass;
         Plain : Prim_Type;
         Primary : PTP := null;
         Composite : TypeList := null;
         Size : Integer := -1;
      end record;

    function PlainType(T : Prim_Type) return PType;
    function PlainTypePtr(T : Prim_Type) return PTP;
    function FuncType(Ret : PTP ; Args : TypeList) return PType;
    function CoordType(T : PTP ; S : Integer) return PType;
    function TupleType(T : TypeList) return PType;
    function ListType(T : PTP) return PType;

    function Works(Sub, Super : PType) return Boolean;
    function Works(Sub, Super : PTP) return Boolean;

    procedure Match(T : PType ; N : in out Node);

    Function Get_Token return String;
    function Node_Inp return Node_Ptr;
    procedure Scene(Elements : Node_List);
    procedure Scene(Element : Node_Ptr);
    function Nd(Kind : Node_Type ; Text : Unbounded_String ; Kids : Node_List) return Node_Ptr;

end POVRay;

package body POVRay is
  function Get_Token return String is
  begin
     return Ada.Text_IO.Get_Line;
  end Get_Token;

  function Node_Inp return Node_Ptr is
     Kind : Node_Type;
     Contents : Unbounded_String;
     Kids_First : Node_List := null;
     Kids_Last : Node_List := null;
  begin
     Kind := Node_Type'Value(Get_Token);
     Contents := To_Unbounded_String(Get_Token);
     while Get_Token = "(" loop
        if Kids_Last = null then
           Kids_Last := new Node_ListImp'(Elem => Node_Inp, Next => null);
           Kids_First := Kids_Last;
        else -- ate a ")"
           Kids_Last.all.Next := new Node_ListImp'(Elem => Node_Inp, Next => null);
        end if;
     end loop;

     return Nd(Kind, Contents, Kids_First);
  end Node_Inp;

  procedure Append(List : Node_List ; Elem : Node_Ptr) is
  begin
     if List.all.Next = null then
        List.all.Next := new Node_ListImp'(Elem => Elem, Next => null);
     else
        Append(List.all.Next, Elem);
     end if;
  end Append;

  procedure Scene(Elements : Node_List) is
     Cur_Node : Node_List := Elements;
  begin
     while Cur_Node /= null loop
        Match(PlainType(Object), Cur_Node.all.Elem.all);
        Cur_Node := Cur_Node.all.Next;
     end loop;
  end Scene;

  procedure Scene(Element : Node_Ptr) is
  begin
     Match(PlainType(Object), Element.all);
  end Scene;

  function Nd(Kind : Node_Type ; Text : Unbounded_String ; Kids : Node_List) return Node_Ptr is
  begin
     return new Node'(NType => Kind, Contents => Text, Children => Kids, Determined_Type => null, Referrent => null);
  end Nd;


    function Find(Name : Unbounded_String; Namespace : Node_Type) return Node_Ptr is
     Cur_Scope : Scope_Ptr := Top_Scope;
     Cur_Var_Def : Var_Def_Ptr := Top_Scope.all.First;
    begin
     while Cur_Scope /= null loop
        while Cur_Var_Def /= null loop
           if Cur_Var_Def.all.Name = Name and Cur_Var_Def.all.Namespace = Namespace then
              return Cur_Var_Def.all.Def;
           end if;
           Cur_Var_Def := Cur_Var_Def.all.Next;
        end loop;
        Cur_Scope := Cur_Scope.all.Up;
     end loop;
     return null; -- not found
    end Find;

    procedure Define(Name : Unbounded_String ; Def : Node_Ptr) is
    begin
        if In_Top_Scope(Name, Def.NType) then
            Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, "Redefining name in same scope.");
        end if;
        Top_Scope.all.First := new Var_Def'(Next => Top_Scope.all.First , Def => Def, Name => Name, Namespace => Def.all.NType);
    end Define;

    function In_Top_Scope(Name : Unbounded_String; Namespace : Node_Type) return Boolean is
     Cur_Var_Def : Var_Def_Ptr := Top_Scope.all.First;
    begin
        while Cur_Var_Def /= null loop
            if Cur_Var_Def.all.Name = Name and Cur_Var_Def.all.Namespace = Namespace then
                return True;
            end if;
            Cur_Var_Def := Cur_Var_Def.all.Next;
        end loop;
        return False; -- not found
    end In_Top_Scope;
    
    procedure Push_Scope is
    begin
        Top_Scope := new Scope'(Up => Top_Scope, First => null);
    end Push_Scope;
    
    procedure Pop_Scope is
    begin
        Top_Scope := Top_Scope.all.Up;
    end Pop_Scope;

  function PlainType(T : Prim_Type) return PType is
  begin
     return (Category => Plain, Primary => null, Plain => T, Composite => null, Size => -1);
  end PlainType;

  function PlainTypePtr(T : Prim_Type) return PTP is
  begin
     return new PType'(Category => Plain, Primary => null, Plain => T, Composite => null, Size => -1);
  end PlainTypePtr;

  function FuncType(Ret : PTP ; Args : TypeList) return PType is
  begin
     return (Category => Func, Plain => Not_Applicable, Primary => Ret, Composite => Args, Size => -1);
  end FuncType;

  function CoordType(T : PTP ; S : Integer) return PType is
  begin
     return (Category => Plain, Primary => T, Plain => Not_Applicable, Composite => null, Size => S);
  end CoordType;

  function TupleType(T : TypeList) return PType is
  begin
     return (Category => Plain, Plain => Not_Applicable, Primary => null, Composite => T, Size => -1);
  end TupleType;

  function ListType(T : PTP) return PType is
  begin
     return (Category => List, Plain => Not_Applicable, Primary => T, Composite => null, Size => -1);
  end ListType;

    function Works(Sub, Super : Prim_Type) return Boolean is
    begin
        return Sub = Super;
    end Works;

    function Works(Sub, Super : TypeList) return Boolean is
        B : TypeList := Sub;
        P : TypeList := Super;
    begin
        while not (B = null or P = null) loop
            if not Works(B.all.Elem.all, P.all.Elem.all) then
                return False;
            end if;
            B := B.all.Next;
            P := P.all.Next;
        end loop;
        return B = null and P = null;
    end Works;

    function Works(Sub, Super : PType) return Boolean is
        begin
            if not (Sub.Category = Super.Category) then
                return False;
            end if;
        if Sub.Category = Plain or Sub.Category = List then
            return Works(Sub.Primary, Super.Primary);
        elsif Sub.Category = Func then
            if not Works(Sub.Primary, Super.Primary) then
                return False;
            end if;
            return Works(Super.Composite, Sub.Composite); --arguments to functions are typed backwards
        elsif Sub.Category = Coord then
            return Works(Sub.Primary, Super.Primary) and Sub.Size = Super.Size;
        elsif Sub.Category = Tuple then
            return Works(Sub.Composite, Super.Composite);
        end if;
        return False;
    end Works;
    
--     function Lambda_Return_Value(N : Node) return Node is
--     begin
--         return N.Children.all.Next.all.Elem.all;
--     end Lambda_Return_Value;
--     function Lambda_Arguments_Value(N : Node) return Node is
--     begin
--         return N.Children.all.Elem.all;
--     end Lambda_Arguments_Value;
    
    function Works(Sub, Super : PTP) return Boolean is
    begin
        return Works(Sub.all, Super.all);
    end Works;
    
    procedure Require(Condition : Boolean ; Message : Standard.String) is
    begin 
        if not Condition then
            Ada.Text_IO.Put_Line(Message);
            Ada.Exceptions.Raise_Exception (Constraint_Error'Identity, Message);
        end if;
    end Require;
    
    procedure Match(T : PType ; N : in out Node) is
        Node_Iter : Node_List;
        Type_Iter : TypeList;
        Count : Integer;
    begin
        if N.Determined_Type /= null then
            Require(N.Determined_Type.all = T, "used as multiple different types");
            return;
        end if;
        
        if N.NType = Float_Literal then --(FloatLiteral <number>)
            Require(T.Category = Plain and T.Plain = Float, "unexpected float literal found");
        elsif N.NType = Coord then --(Coord - <d1> <d2> <d3> ...)
            Require(T.Category = Coord, "unexpected coord found");
            Node_Iter := N.Children;
            Count := 0;
            while not (Node_Iter = null) loop
                Count := Count + 1;
                Match(T.Primary.all, Node_Iter.Elem.all);
                Node_Iter := Node_Iter.all.Next;
            end loop;
            Require(Count = T.Size, "coord has wrong size");
        elsif N.NType = Tuple then --(Tuple <e1> <e2> <e3> ...)
            Require(T.Category = Tuple, "unexpected tuple expression found");
            Type_Iter := T.Composite;
            Node_Iter := N.Children;
            while Node_Iter /= null loop
                Require(Type_Iter /= null, "too many elements in tuple");
                Match(Type_Iter.all.Elem.all, Node_Iter.all.Elem.all);
                Type_Iter := Type_Iter.all.Next;
                Node_Iter := Node_Iter.all.Next;
            end loop;
            Require(Type_Iter = null, "too few arguments in tuple");
        elsif N.NType = List then --(List <e1> <e2> <e3> ...)
            Require(T.Category = List, "unexpected list found");
            Node_Iter := N.Children;
            while Node_Iter /= null loop
                Match(T.Primary.all,Node_Iter.all.Elem.all);
                Node_Iter := Node_Iter.all.Next;
            end loop;
        elsif N.NType = Lambda then --(Lambda (X arg1 arg2 ...) <expr>)
            Require(T.Category = Func, "lambda expression found, but function not expected");
            Push_Scope;
            Type_Iter := T.Composite;
            Node_Iter := N.Children.all.Elem.all.Children; --iterate over 1st child, the arguments
            while Node_Iter /= null loop
                Require(Type_Iter /= null, "too many arguments in lambda expression");
                Node_Iter.all.Elem.all.Determined_Type := Type_Iter.all.Elem.all;
                Define(Node_Iter.all.Elem.all.Contents, Node_Iter.all.Elem.all); --add name to scope
                Type_Iter := Type_Iter.all.Next;
                Node_Iter := Node_Iter.all.Next;
            end loop;
            Require(Type_Iter = null, "too few arguments in lambda expression");
            Match(T.Primary.all, N.Children.all.Next.all.Elem.all); --2nd child of the node, the expression
            Pop_Scope;
        elsif N.NType = Def_Var or N.NType = Def_MVar then --(Def_M?Var:<name>)
            Require(T.Category = Plain and T.Plain = Action, "unexpected definition in functional context");
            Define(N.Contents, N.Children.all.Elem); --first and only child
        elsif N.NType = Def_Proc then --(Def_Proc
            Define(N.Contents, N.Children.all.Elem); --will be sorted out by the procedure call
        elsif N.NType = Block then
            Push_Scope;
            Require(T.Category = Plain and T.Plain = Action, "unexpected block");
            Node_Iter := N.Children;
            while Node_Iter /= null loop
                Match(T, Node_Iter.all.Elem.all);
                Node_Iter := Node_Iter.all.Next;
            end loop;
            Pop_Scope;
        elsif N.NType = Var_Ref or N.NType = MVar_Ref or N.Ntype = P_Ref then
            N.Referrent := Find(N.Contents, N.NType);
            Require(N.Referrent /= null, "unresolved name: '" & To_String(N.Contents) & "'");
            Match(T, N.Referrent.all);
        end if;
        --would rather alias, but can't figure out how to do it without using ptrs everywhere
        N.Determined_Type := new PType'(Category => T.Category, Plain => T.Plain,
            Primary => T.Primary, Composite => T.Composite, Size => T.Size);
    end Match;

--     --names can be introduced by Lambda, Def, MDef, and Proc_Def
--     --scopes can be introduced  Lambda, Block, and Proc_Def
--     type Node_Type is (Func_Call, -- fn (expr) args (tuple)
--                       Float_Literal, String_Literal,
--                       Lambda, -- args (names) val (expr)
--                       Coord, Tuple, List, -- different composite types
--                       Def, -- name (name) val (expr)
--                       MDef, -- name (name) val (expr)
--                       Block, -- action (proc) next (block)
--                       Proc, -- name (name) largs (names) rargs (exprs)
--                       Proc_Def, -- name (name) largs (names) rargs (names) body (block)
--                       Var_Ref, MVar_Ref, P_Ref); -- name in contents

end POVRay;

use POVRay;
begin
   if not(Get_Token = "(") then
      Ada.Text_IO.Put("Uh, oh.");
   end if;
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
