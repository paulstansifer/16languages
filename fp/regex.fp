{dot %-1}
{star %-2}

#Dang, first-class functions would be really neat.
{
  match #<pat string>
  (
    null@2 -> null@1 ;
    or@[startmatch match@[1 tl@2]])
}

{
  startmatch #<pat string>
  (
    or@[null@1 null@2] ->
      null@1   #if the pattern's empty, it's a match
               #otherwise, it's not.
    ;
    #does this character match?
    (atom@1@1 ->
      (or@[
            =@[1@1 dot]
            =@[1@1 1@2] 
          ] -> startmatch@[tl@1 tl@2]
          ; =@[%0 %1] #no match, return a fake F
      )
      ; # it's an alternation
      |or@&(startmatch)@distr@[&concat@&([[1] 2])@distr@[1@1 tl@1]  2]
    )
  )
}


startmatch:< <> <> >
startmatch:< <> <1> >
startmatch:< <1> <1> >
startmatch:< <1> <1 2> >
startmatch:< < <1 2> 3 4 5 > <1 3 4 5> >


match:< <2> <1 2 3> >
match:< <1 2 3> <1 2 3> >
match:< <2 3 4> <1 2 3 4 5> >
match:< < <1 2> <3 4> <5 6> >  <1 3 5> >
match: < < 1 2 <3 4 5 6 > 4 >   <1 2 3 4> >
match:< < <1 2> <3 4> >  <1 2 3 4> >

startmatch:< <2> <1 2> >
match:< <4> <1 2 3> >
match:< <1 2> <1> >
match:< <1 2 3> <3 2 1 2> >
match:< < <1 2> <4 5> >  <1 2 3 4> >
