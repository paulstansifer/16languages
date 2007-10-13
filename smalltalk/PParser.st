"Filed out from GNU Smalltalk version 2.95d on 7-Oct-2007 21:41:28"

"Object subclass: PParser [
    | pre lists |
    
    <category: nil>
    <comment: nil>
"
Object subclass: #PParser
	instanceVariableNames: 'curEnv lists tags'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !



!PParser methodsFor: 'general'!
    emit: text
	Transcript show: text.
!
    init
	curEnv := nil.
	lists := OrderedCollection new.
	tags := OrderedCollection new.
	tags add: (ParserTagState new init: 'tt' enterSyn: '[=' leaveSyn: '=]');
	     add: (ParserTagState new init: 'i' enterSyn: '///' leaveSyn: '///');
	     add: (ParserTagState new init: 'b' enterSyn: '***' leaveSyn: '***');
	     add: (ParserTagState new init: 'strike' enterSyn: '-s-' leaveSyn: '-s-');
	     add: (ParserWPLink new init);
	     add: (ParserPlain new init: '&mdash;' syn: '---');
	     add: (ParserPlain new init: '&rarr;' syn: '-->');
	     add: (ParserPlain new init: '&larr;' syn: '<--');
	     add: (ParserPlain new init: '&harr;' syn: '<->').
!
    listFromMarker: marker
	marker = $* ifTrue: [^ #UL].
	marker = $# ifTrue: [^ #OL].
	^nil.
!
    unwindUntil: pos
	(lists size) to: pos by: -1 do: [: idx|
		Transcript show: '</', (self tagFromList: lists removeLast), '>'.
	].
!
    environFromMarker: marker
	marker = $  ifTrue: [^ 'pre'].
	marker = $^ ifTrue: [^ 'blockquote'].
	^nil.


!
    parseLine: line
	| first inLists charPosition envTag |
	charPosition := 0.
	first := line at: 1.

	envTag := self environFromMarker: first.

	envTag = nil ifTrue: [
	    curEnv = nil ifFalse: [Transcript show:'</',curEnv,'>'. curEnv := nil.].
	] ifFalse: [
		curEnv = nil ifTrue: [ 
		    Transcript show:'<', envTag ,'>'.
		] ifFalse: [ 
		    curEnv = envTag ifFalse: [
			Transcript show: '</', curEnv, '><', envTag, '>'.
		    ]].
		curEnv := envTag.

		curEnv = 'pre' ifTrue: [ "preformatted text is taken literally"
		    Transcript showCr: (line allButFirst).
		    ^nil
		] ifFalse: [
		    charPosition := charPosition + 1
		].
	].

	inLists := true.

	[inLists & (charPosition+1 <= line size)] whileTrue: [
	    | char |
	    charPosition := charPosition + 1.		
	    char := line at: charPosition.
	    inLists ifTrue: [
		| listType |
		listType := self listFromMarker: char.
		inLists := (listType = nil) not.

		(lists size < charPosition 
                   or: [((lists at: charPosition) = listType) not ])  ifTrue: [
		       "The rest of the symbols (if any) are no longer relevant"
		       self unwindUntil: charPosition.
		   ] .			
		
		(listType = nil) not & (lists size < charPosition) ifTrue: [
		    lists add: listType.
		    Transcript show: '<', (self tagFromList: listType), '>'.
		].

		"ending the stack of list symbols?"
		inLists not & (lists size > 0) ifTrue: [Transcript show: '<li>'.].
	    ].

	].


	[charPosition <= line size] whileTrue: [
	    | match tagidx |
    
            match := false.
	    tagidx := 1.
	    [match not & (tagidx <= tags size)] whileTrue: [
		| dist |
		dist := (tags at: tagidx) try: line at: charPosition.
		dist > 0 ifTrue: [
		    match := true.
		    charPosition := charPosition + dist.
		].
		tagidx := tagidx + 1.
	    ].

	    match ifFalse: [
		Transcript show: (self sub: (line at: charPosition)).
		charPosition := charPosition + 1.
	    ].

	].
	lists size > 0 ifTrue: [ Transcript show: '</li>'. ].
	Transcript cr.
!
    sub: char
	char = $< ifTrue:[^'&lt;'].
	char = $> ifTrue:[^'&gt;'].
	^char asString.
!
    tagFromList: list
	list = #UL ifTrue: [^ 'ul'].
	list = #OL ifTrue: [^ 'ol'].
!!


Object subclass: #ParserTagState
	instanceVariableNames: 'state enter leave tag'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !

! ParserTagState methodsFor: 'general' !
    init: stateTag enterSyn: enterSyn leaveSyn: leaveSyn
	state := false.
	tag := stateTag.
	enter := enterSyn.
	leave := leaveSyn.
!
    try: line at: pos
	state ifTrue: [ ^self tryLeave: line at: pos ]
	    ifFalse: [ ^self tryEnter: line at: pos ].
!
    tryEnter: line at: pos
	(line subMatch: enter startingAt: pos)
	    ifTrue: [ state := true. Transcript show: '<', tag, '>'.  ^(enter size)].
	^0.
!
    tryLeave: line at: pos
	(line subMatch: leave startingAt: pos)
	    ifTrue: [ state := false. Transcript show: '</', tag, '>'. ^(leave size)].
	^0.
!!

! SequenceableCollection methodsFor:'testing' !
    subMatch: subCollection startingAt: idx
	| subIndex |
	(idx + subCollection size - 1) > self size ifTrue:[^false].
	subIndex := 1.
	self from: idx to: (idx + subCollection size - 1) do: 
	    [:char |
		(char = (subCollection at: subIndex))
		    ifFalse: [^false].
		subIndex := subIndex + 1.
	    ].
	^true.
!!

ParserTagState subclass: #ParserWPLink 
	instanceVariableNames: 'startPos'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !

! ParserWPLink methodsFor: 'general' !
    init
	state := false.
!
    tryEnter: line at: pos
	(line subMatch: '[[' startingAt: pos)
            ifTrue: [ 
		state := true. startPos := pos + 2. 
		^((line indexOfRegex:'\]\]' startingAt: pos ifAbsent:[pos]) first - pos)
		].
	^0.
!
    tryLeave: line at: pos
	(line subMatch: ']]' startingAt: pos)
	    ifTrue: [ 
		|dest|
		state := false.
		dest := line copyFrom: startPos to: pos - 1.
		Transcript show: '<a href="http://en.wikipedia.org/wiki/', 
		    (dest collect: [:c|c=$  ifTrue:[$_] ifFalse:[c]]),
		    '">', dest, '</a>'.
		^2].
	^0.
!!

Object subclass: #ParserPlain
	instanceVariableNames: 'syntax element'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !

! ParserPlain methodsFor: 'general' !
    init: html syn: syn
	syntax := syn.
	element := html.
!
    try: line at: pos
	(line subMatch: syntax startingAt: pos)
	    ifTrue: [ Transcript show: element.  ^(syntax size)].
	^0.
!!


p := PParser new.
p init.
"
p parseLine: 'Normal line'.
p parseLine: ' preformatted line'.
p parseLine: ' ...or lines'.
p parseLine: '^quoted line'.
p parseLine: '*bullet1'.
p parseLine: '**bullet2'.
p parseLine: 'Normal line'.
p parseLine: '-s-stuck out-s- arr-->ow [=typewriter ///ital/// still typewriter=]'.
p parseLine: '* [[Wikipedia]]'.
p parseLine: 'my favorite punctuation --- the dash.'.
"

in := FileStream stdin.

[in atEnd not] whileTrue: [ p parseLine: in nextLine. ].
p parseLine: ''. "close all list tags."

