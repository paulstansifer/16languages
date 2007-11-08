{
  abs
  (>@[id %0] -> id ; -@[%0 id])
}


{
  sqrt  #by Newton's Method
  2@(while >@[abs@-@[2 3] %0.0001]
    [1 *@[%0.5 +@[2 /@[1 2]]] 2]
  )@[id id %0]
}

{
  f
  +@[sqrt@abs |*@[%5 id id id]]
} 

&([1 (>@[2 %400] -> %0 ; 2)])@
&([1 f@2])@reverse@&([id *@[id %0.45]])@iota:11

