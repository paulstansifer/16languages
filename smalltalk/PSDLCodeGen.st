" Here's the plan.  I'm not writing another parser, so the object tree will be represented as Smalltalk code.

Materials.  Every object may have at most one finish, interior, normal, and pigment.
  No buckets like _texture_ and _material_ are allowed.
"



Object subclass: #PFunnyNester
        instanceVariableNames: 'declaredName transforms children'
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !

PFunnyNester class
	instanceVariableNames: 'MasterList' !

! PFunnyNester class methodsFor: 'init' !
    add: elem
	MasterList = nil ifTrue: [
	    MasterList := OrderedCollection new.
	].
	MasterList add: elem.
!!

! PFunnyNester methodsFor: 'codegen' !
    init: name
	declaredName := name. "set to nil to place literally."
	transforms := OrderedCollection new.
	children := OrderedCollection new.

	PFunnyNester add: self.
!
    emitNested
	^ self tag, '{', self emitNaked, '}'.
!
    emitNaked
	declaredName = nil ifTrue: [
	    |retVal|
	    retVal := self emitCore.
	    transforms do: [ :t|
		retVal := retVal, t emit.
	    ].
	] ifFalse: [
	    ^declaredName.
	].
!
    emitInFlatContext
	declaredName = nil ifTrue: [
	    |retVal|
	    retVal := ''.
	    transforms doReverse: [ :t|
		retVal := retVal, t inverse emit.
	    ].
	
	    retVal := retVal, self emitNaked.
	    ^retVal.
	] ifFalse: [
	    ^declaredName.
	].
!
    emitCore
	^self emitChildren.
!
    emitChildren
	|retVal|
        retVal := ''.
	children do: [ :c|
	    (c class = self class) | c reallyNests  ifTrue:[
		retVal := retVal, c emitFlat.
	    ] ifFalse: [
		retVal := retVal, c emitNested.
	    ].
	].
	^retVal.
!
    addChild: c
	children add: c
!
    transform: t
	transforms add: t
!!

PFunnyNester subclass: #PObject
        instanceVariableNames: 'tag body pigment finish normal interior'
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !
PObject class instanceVariableNames: 'lastId' !
!PObject class methodsFor: 'uid'!
    getAName
	lastId = nil ifTrue: [
	    lastId := 0.
	].
	lastId := lastId + 1.
	^ 'o', lastId printString.
!!

!PObject methodsFor: 'codegen'!
    init: name tag: t body: b
	super init: name.
	tag := t.
	body := b.
!
    reallyNests
	^false
!
    finish: f
	finish := f.
!
    normal: n
	normal := n.
!
    pigment: p
	pigment := p.
!
    interior: i
	interior := i.
!
    emitCore
	|retVal|
        retVal :=  body.
	
	finish class = PMapped ifTrue: [
	    " POV-Ray doesn't have finish_maps, but it does have texture_maps. "
	    " Thank goodness there's only one thing missing maps. "
	    | pname nname |
	    
	    pigment = nil ifFalse: [
		pname := PObject getAName.
		retVal := retVal, '#declare ',pname,'=', pigment emitNested.
	    ] ifTrue: [pname := nil].
	    normal = nil ifFalse: [
		nname := PObject getAName.
		retVal := retVal, '#declare ',nname,'=', normal emitNested.
	    ] ifTrue: [nname := nil].
	    
	    ^ retVal, (finish emitFinishHackPname: pname nname: nname).
	] ifFalse: [
	    retVal :=  'texture{'.
	    finish = nil  ifFalse:[retVal := retVal, finish  emitNested].
	    normal = nil  ifFalse:[retVal := retVal, normal  emitNested].
	    pigment = nil ifFalse:[retVal := retVal, pigment emitNested].
	    ^ retVal, '}'.
	].
!
    tag
	^tag
!!

PObject subclass: #PCSG
        instanceVariableNames: ''
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !
!PCSG methodsFor: 'codegen'!
    init: tag
	super init: nil tag: tag  body: ''.
!
    emitCore
	"CSG really does have children"
	self emitChildren
!!




PFunnyNester subclass: #PMapped
        instanceVariableNames: 'type pattern locations elements'
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !
!PMapped class methodsFor: 'codegen'!
    isAMappable: tag
	^ tag = 'pigment' | tag = 'finish' | tag = 'normal'
!!

!PMapped methodsFor: 'codegen'!
    tag
	^type
	"
	type = #texture ifTrue: [^'texture'].
	type = #pigment ifTrue: [^'pigment'].
	type = #finish  ifTrue: [^'finish'].
	type = #normal  ifTrue: [^'normal'].
	"
!
    init: name
	super init: name.
!
    init: t pat: pat
	super init: nil.
	type := t.
	pattern := pat.
	locations := OrderedCollection new.
	elements := OrderedCollection new.
!
    emitCore
	| retVal |
        retVal := pattern emit, ' '.

	type = 'normal' ifTrue: [ retVal := retVal, '1 ' ].
	"To control normal depth, a map must be used."

	locations size > 0 ifTrue: [
	    
	    type = 'normal' ifTrue: [
		retVal := retVal, 'slope_map{'.
	    ] ifFalse: [
		retVal := retVal, self tag, '_map{'.
	    ].
	
	    1 to: locations size do: [ :i|
		retVal := retVal, '[', (locations at: i) printString, ','
		    , (elements at: i) emitNaked, ']'.
	    ].
	    retVal := retVal, '}'.
	].
	^retVal
!  """"""
    emitFinishHackPname: pn nname: nn
	| retVal |
        retVal := 'texture{', pattern emit, ' texture_map{'.
	
	1 to: locations size do: [ :i|
	    | thisfinish |
	    retVal := retVal, '[', (locations at: i) printString, ','.
	    thisfinish := elements at: i.
	    thisfinish class = PMapped ifTrue: [
		retVal := retVal, (thisfinish emitFinishHackPname: pn nname: nn).
	    ] ifFalse: [
		pn = nil ifFalse: [
		    retVal := retVal, 'pigment{', pn, '}'.
		].
		nn = nil ifFalse: [
		    retVal := retVal, 'normal{', nn, '}'.
		].
		(elements at: i) emitNested.
	    ].
	    retVal := retVal, ']'.
	].
	^retVal, '}}'.
	    
!
    add: elem at: loc
	locations add: loc.
	elements add: elem.
!!


Object subclass: #PNormalMapPair
        instanceVariableNames: 'left right' " heck if I can remember which number stands for what "
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !

!PNormalMapPair methodsFor: 'codegen'!
    initLeft: l right: r
	left := l.
	right := r.
!
    emit
	^ '<', left printStr, ',', right printStr, '>'.
!!


Object subclass: #PCombinedTransform
        instanceVariableNames: 'first rest'
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !
!PCombinedTransform methodsFor: 'codegen'!
    initFirst: f rest: r
	first := f.
	rest := r.
!
     emit
	 ^first emit, rest emit.
!
     inverse
	 ^PCombinedTransform new initFirst: (rest inverse) rest: (first inverse).
!!

Object subclass: #PPrimitiveTransform
        instanceVariableNames: 'type x y z'
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !
!PPrimitiveTransform methodsFor: 'codegen'!
    init: t x: xloc y: yloc z: zloc
	type := t. x := xloc. y := yloc. z := zloc.
!
    emit
	^type, '<',x printString,',',y printString,',',z printString,'> '.
!
    inverse
	type = 'scale' ifTrue: [^PPrimitiveTransform new init: 'scale' x: 1/x y: 1/y z: 1/z].
	type = 'translate' ifTrue: [^PPrimitiveTransform new init: 'scale' x: 0-x y: 0-y z: 0-z].
	type = 'rotate' ifTrue: [
	    "POV-Ray rotates the three dimensions in the given order."
	    ^PCombinedTransform new
		initFirst:
		    (PPrimitiveTransform new init: 'rotate' x: 0 y: 0 z: 0-z)
		rest:
		    (PCombinedTransform new initFirst:
			(PPrimitiveTransform new init: 'rotate' x: 0 y: 0-y z: 0)
					    rest:
			(PPrimitiveTransform new init: 'rotate' x: 0-x y: 0 z: 0))
	].
!!

Object subclass: #PFunction
        instanceVariableNames: 'contents'
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !

!PFunction methodsFor: 'codegen'!
    init: c
	contents := c.
!
    emit "by default, emit as a pattern"
	^'function{', self emitFn, '}'.
!
    emitFn
	^contents emit.
!!


Object subclass: #PPattern
        instanceVariableNames: 'name transforms'
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !

!PPattern methodsFor: 'codegen'!
    init: n
	name := n.
	transforms := OrderedCollection new.
!
    emit
	|retVal|
	retVal := name emit.
	transforms do: [:t|
	    retVal := retVal, ' ', t emit.
	].
	^retVal
!
    emitFn 
	^'pattern{', self emit, '}'.
!
    transform: t
	transforms add: t.
!!



"Fallback -- this is a place where Smalltalk is really neat.
 This way, I can use a string in the place of an object I can't 
 programmatically emit yet."
!String methodsFor: 'PSDLcodegen'!
    emit
	^self
!
    emitNested
	^self.
!
    emitNaked
	^self.
	"Actually, this had better not be called along with 'emit',
	 'cause only one of them could possibly be valid."
!!

| red yellow |
red := 'pigment{color rgb <1,0,0>}'.
yellow := 'pigment{color rgb <1,1,0>}'.

p := PObject new init: nil tag: 'sphere' body: '<0,0,0> 1'
    ; pigment: red 
    ; finish: (PMapped new init: 'finish' pat: (PPattern new init: 'bozo' ; 
						    transform: 'warp{turbulence 0.1}' )  ;
		    add: 'finish{ambient 1}' at: 0.2 ;
		    add: 'finish{ambient 0.2}' at: 0.4 )
    ; transform: (PPrimitiveTransform new init: 'rotate' x: 45 y: 30 z:16) inverse.
Transcript show: (p emitNested).

c := PCSG new.
c init: 'union'.
c addChild: p ; addChild: p.
Transcript show: (c emitNested).
