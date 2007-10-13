"Filed out from GNU Smalltalk version 2.95d on 7-Oct-2007 21:55:33"

Object subclass: Point [
    | x y |
    
    <category: 'Language-Data types'>
    <comment: 'Beginning of a Point class for simple display manipulation.  Has not been
 exhaustively tested but appears to work for the basic primitives and for
 the needs of the Rectangle class.'>

    Point class >> new
    "Create a new point with both coordinates set to 0"
    ^self basicNew x: 0 y: 0

    Point class >> x: xInteger y: yInteger
    "Create a new point with the given coordinates"
    ^self basicNew x: xInteger y: yInteger


    x
    "Answer the x coordinate"
    ^x

    x: aNumber
    "Set the x coordinate to aNumber"
    x := aNumber

    x: anXNumber y: aYNumber
    "Set the x and y coordinate to anXNumber and aYNumber, respectively"
    x := anXNumber.
    y := aYNumber

    y
    "Answer the y coordinate"
    ^y

    y: aNumber
    "Set the y coordinate to aNumber"
    y := aNumber

    * scale
    "Multiply the receiver by scale, which can be a Number or a Point"
    | scalePoint |
    scalePoint := scale asPoint.
    ^Point x: (self x * scalePoint x) y: (self y * scalePoint y)

    + delta
    "Sum the receiver and delta, which can be a Number or a Point"
    | deltaPoint |
    deltaPoint := delta asPoint.
    ^Point x: (self x + deltaPoint x) y: (self y + deltaPoint y)

    - delta
    "Subtract delta, which can be a Number or a Point, from the receiver"
    | deltaPoint |
    deltaPoint := delta asPoint.
    ^Point x: (self x - deltaPoint x) y: (self y - deltaPoint y)

    / scale
    "Divide the receiver by scale, which can be a Number or a Point, with
     no loss of precision"
    | scalePoint |
    scalePoint := scale asPoint.
    ^Point x: (self x / scalePoint x) y: (self y / scalePoint y)

    // scale
    "Divide the receiver by scale, which can be a Number or a Point, with
     truncation towards -infinity"
    | scalePoint |
    scalePoint := scale asPoint.
    ^Point x: (self x // scalePoint x) y: (self y // scalePoint y)

    abs
    "Answer a new point whose coordinates are the absolute values of the
     receiver's"
    ^Point x: (self x abs) y: (self y abs)

    < aPoint
    "Answer whether the receiver is higher and to the left of aPoint"
    ^(self x < aPoint x) and: [ (self y < aPoint y) ]

    <= aPoint
    "Answer whether aPoint is equal to the receiver, or the receiver
     is higher and to the left of aPoint"
    ^(self x <= aPoint x) and: [ (self y <= aPoint y) ]

    = aPoint
    "Answer whether the receiver is equal to aPoint"
    ^(aPoint class == Point) and: [ (self x = aPoint x) & (self y = aPoint y) ]

    > aPoint
    "Answer whether the receiver is lower and to the right of aPoint"
    ^(self x > aPoint x) and: [ (self y > aPoint y) ]

    >= aPoint
    "Answer whether aPoint is equal to the receiver, or the receiver
     is lower and to the right of aPoint"
    ^(self x >= aPoint x) and: [ (self y >= aPoint y) ]

    max: aPoint
    "Answer self if it is lower and to the right of aPoint, aPoint otherwise"
    ^(self x max: aPoint x) @ (self y max: aPoint y)

    min: aPoint
    "Answer self if it is higher and to the left of aPoint, aPoint otherwise"
    ^(self x min: aPoint x) @ (self y min: aPoint y)

    asPoint
    ^self			"But I already AM a point!"

    asRectangle
    "Answer an empty rectangle whose origin is self"
    ^Rectangle origin: self corner: self copy

    corner: aPoint
    "Answer a Rectangle whose origin is the receiver and whose corner
     is aPoint"
    ^Rectangle origin: self corner: aPoint

    extent: aPoint
    "Answer a Rectangle whose origin is the receiver and whose extent
     is aPoint"
    ^Rectangle origin: self extent: aPoint

    hash
    "Answer an hash value for the receiver"
    ^self x hash bitXor: self y hash

    arcTan
    "Answer the angle (measured counterclockwise) between the receiver and
     a ray starting in (0, 0) and moving towards (1, 0) - i.e. 3 o'clock"

    ^self y arcTan: self x

    dist: aPoint
    "Answer the distance between the receiver and aPoint"
    | a b |
    a := self x - aPoint x.
    b := self y - aPoint y.
    ^((a squared) + (b squared)) sqrt

    dotProduct: aPoint
    "Answer the dot product between the receiver and aPoint"
    ^(self x * aPoint x) + (self y * aPoint y)

    grid: aPoint
    "Answer a new point whose coordinates are rounded towards the nearest
     multiple of aPoint"
    ^Point 
	x: (self x roundTo: (aPoint x))
	y: (self y roundTo: (aPoint y))

    normal
    "Rotate the Point 90degrees clockwise and get the unit vector"
    | len |
    len := ((self x squared) + (self y squared)) sqrt.
    ^Point x: (self y negated / len) y: (x / len)

    transpose
    "Answer a new point whose coordinates are the receiver's coordinates
     exchanged (x becomes y, y becomes x)"
    ^Point x: y y: x

    truncatedGrid: aPoint
    "Answer a new point whose coordinates are rounded towards -infinity,
     to a multiple of grid (which must be a Point)"
    ^Point
	x: (self x truncateTo: (aPoint x))
	y: (self y truncateTo: (aPoint y))

    printOn: aStream
    "Print a representation for the receiver on aStream"
    aStream print: x;
	nextPut: $@;
	print: y

    storeOn: aStream
    "Print Smalltalk code compiling to the receiver on aStream"
    aStream nextPut: $(;
	store: x;
	nextPutAll: ' @ ';
	store: y;
	nextPut: $)

    rounded
    "Answer a new point whose coordinates are rounded to the nearest integer"
    ^Point x: (self x rounded) y: (self y rounded)

    truncateTo: grid
    "Answer a new point whose coordinates are rounded towards -infinity,
     to a multiple of grid (which must be a Number)"
    ^Point x: (self x truncateTo: grid) y: (self y truncateTo: grid)
]

