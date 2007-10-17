Object subclass: #ActionPoint
	instanceVariableNames: 'name temperature'
	classVariableNames: 'rand'
	poolDictionaries: ''
	category: nil !

! ActionPoint methodsFor: 'general' !
    tick: outside
	rand = nil ifTrue: [ rand := Random new. ].
	temperature := ((0.9 * temperature) + (0.1 * outside) + rand next) * 0.9.

!!

ActionPoint subclass: #System
	instanceVariableNames: 'name planets governments technology'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !

! System methodsFor: 'general' !
    tick: outside
	super tick: outside.

	maxT := 0
	planets do: [:p| maxT := (p tick) max: maxT].
	governments do: [:g| maxT := (g tick) max: maxT].

	temperature := temperature + (0.1*maxT).

	^temperature
!

!!

Object subclass: #Government
	instanceVariableNames: 'name reputations'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !

Object subclass: #Planet
	instanceVariableNames: 'name places'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !



Object subclass: #Place
	instanceVariableNames: 'population crime government policePresence'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !

! Place methodsFor: 'general' !
    transactWith: organization

!!
Government subclass: #LocalCrimeOrg
	instanceVariableNames: 'member'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !


Object subclass: #Reputation
	instanceVariableNames: 'other suspicion trust'
	classVariableNames: ''
	poolDictionaries: ''
	category: nil !



