Object subclass: #PSDLCodeGen
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !
! PSDLCodeGen methodsFor: 'parsing' !
    parse: tokens
	| idx stack |
        idx := 1.
	stack := OrderedCollection new.
	[tokens atEnd not] whileTrue: [
	    | t |
	    t := tokens next.
	    t = '(' ifTrue: [
		stack add: (pick: tokens).
	    ].
	    t = ')' ifTrue: [
		| done |
		done := stack removeLast.
		stack size > 0 ifTrue: [
		    stack last dealWith: done.
		] ifFalse: [
		    done emitNested.
		].
	    ].
	]
!
    pick: tokens
	| marker name |
	marker := tokens next.

	marker = 'named' ifTrue: [
	    name := tokens next.
	    marker := tokens next. " wacky syntax, but easy to parse "
	].

	marker = 'texture' ifTrue: [
	    ^ texture new: name.
	]
	
!!



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

! PFunnyNester methodsFor: 'codegen' !
    init: name
	declaredName := name. "set to nil to place literally."
	transforms := OrderedCollection new.
	children := OrderedCollection new.

	PFunnyNester add: self.
!
    emitNested
	Transcript show: self tag, '{'.
	declaredName = nil ifTrue: [
	    self emitCore.
	    transforms do: [ :t|
		t emit.
	    ].
	] ifFalse: [
	    Transcript show: declaredName.
	].
	Transcript show: '}'.
!
    emitInFlatContext
	declaredName = nil ifTrue: [
	    
	transforms doReverse: [ :t|
	    t inverse emit.
	].
	
	self emitCore.

	transforms do: [ :t|
	    t emit.
	].
!
    emitCore
	self emitChildren.
!
    emitChildren
	children do: [ :c|
	    (c class = self class) | c reallyNests  ifTrue:[
		c emitFlat.
	    ] ifFalse: [
		c emitNested.
	    ].
	].
!
    addTransform: t
	transforms add: t
!!

PFunnyNester subclass: #PMaterial
        instanceVariableNames: ''
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !

!PMaterial methodsFor: 'codegen'!
    init: name tex: tex int: int
	super init: name.
!
    reallyNests
	^false
!
    tag
	^'material'
!!



!Texture methodsFor: 'codegen'!
    tag
	^'texture'
!!

PFunnyNester subclass: #PMappable
        instanceVariableNames: 'type pattern map'
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !

!PMappable methodsFor: 'codegen'!
    tag
	type = #texture ifTrue: [^'texture'].
	type = #pigment ifTrue: [^'pigment'].
	type = #finish  ifTrue: [^'finish'].
	type = #normal  ifTrue: [^'normal'].
	
	^tag
!
    emitCore
	(pattern = nil) ifTrue: [`
	    self emitOther.
	] ifFalse: [
	    pattern emit.
	    map emit.
	].
!!




Object subclass: #PMap
        instanceVariableNames: 'locations elements'
        classVariableNames: ''
        poolDictionaries: ''
        category: nil !

!PMap methodsFor: 'codegen'!
    add: elem at: loc
	locations add: loc.
	elements add: elem.
!
    emit
	Transcript show: '[', loc printString, ','.
	elem emitNested.
	Transcript show: ']'.
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
	Transcript show: '<', left printStr, ',', right printStr, '>'.
!!



