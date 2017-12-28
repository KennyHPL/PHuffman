#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"hzip.st"
"Kenny Luu"
"kepluu@ucsc.edu"
"asg 3"

execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.

"----------------------------------------"
"Utility code from example files"
"----------------------------------------"

"catFile.st"
FileStream extend [
   nl := Character nl.
   catFile [
      |count textline|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         textline :=self nextLine.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << textline << nl.
      ]
   ]
]

"charfreq.st"
Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger ]
   ]
]

"tree.st"
Object subclass: Leaf [
   |char count isLeaf|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [^ isLeaf ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      isLeaf := true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: visitor [
      visitor  value:char value: isLeaf.
   ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isLeaf := false.
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorder: visitor[      
      left postorder: visitor .
      right postorder: visitor .  
      visitor value: char value:isLeaf.
   ]
]

"BitStream.st"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

"OutBits.st"
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]
"----------------------------------------"
"Main function"
"----------------------------------------"
|inFile readFile outFile decompressed fileName argc|
argc:=0.
pqueue := SortedCollection new.
"getOpt"
cOpt := false.
dOpt := false.
tOpt := false.
uOpt := false.

Smalltalk arguments: '-c -d -t -u'
do:[:opt :arg|
    (opt = $c) ifTrue: [cOpt := true.].
    (opt = $d) ifTrue: [dOpt := true.].
    (opt = $t) ifTrue: [tOpt := true.].
    (opt = $u) ifTrue: [uOpt := true.].

    (arg ~= nil) ifTrue:[
        argc = 0 ifTrue:[
            dOpt ifTrue:[stdout <<'inFile: '<<arg<<nl.].
            inFile := FileStream open: arg mode: FileStream read.
            readFile := FileStream open: arg mode: FileStream read.
            dOpt ifTrue:[stdout <<'argc: '<<argc<<nl.].
        ].
        argc = 1 ifTrue:[
            dOpt ifTrue:[stdout <<'outFile: '<<arg<<nl.].
            outFile := FileStream open: arg mode: FileStream write.
            dOpt ifTrue:[stdout <<'argc: '<<argc<<nl.].
        ].
        argc > 1 ifTrue:
            [stderr <<'Usage: hzip.st -cdtu inFile [outFile]'<<nl].

        argc := argc +1.
    ].
].

"----------------------------------------"
"Compression mode"
"----------------------------------------"
(cOpt|tOpt) ifTrue:[
    dOpt ifTrue:[stdout << 'Compression'<<nl.].
    
    freqtable := ZeroArray new: 257.
    freqtable at: 256 put: 1.

    "Inserting each char into a freqtable"
    [inFile atEnd not] whileTrue:[
        |ordChar|
        ordChar := inFile next asInteger.
        freqtable incr: ordChar.
    ].

    dOpt ifTrue:[
        freqtable keysAndValuesDo: [:key :value |
            (value notNil and: [value > 0]) ifTrue: [
            stdout << 'freq[' << key asCharacter visible << ']=' <<value
                << nl.
            ]
        ]
    ].

    "Inserting the freqtable into a priority queue"
    freqtable keysAndValuesDo: [:key :value|
        (value notNil and: [value > 0]) ifTrue:[
            pqueue add: (Leaf new: key count: value).
        ]
    ].

    dOpt ifTrue:[
        pqueue inspect. 
    ].

    "Using the huffman algorithm to make an encoding tree"

    encTable := ZeroArray new: 257.

    [pqueue size > 1] whileTrue:[
        |leftc rightc|
        leftc := pqueue removeFirst.
        rightc := pqueue removeFirst.

        newtree := (Tree new: (leftc char)
                              count: (leftc count) + (rightc count)
                              left: leftc
                              right: rightc).
        pqueue add: newtree.
    ].

    encTree := pqueue removeFirst.

    encTree inorder: [:char :string|
        encTable at: char put: string.
    ]prefix: ''.

    dOpt ifTrue:[encTree inspect.].

    encString :=''.

    "Building the encoding string"
    encTree postorder: [:char :isLeaf |
        isLeaf ifFalse:[encString := encString, '1'.]
               ifTrue:[
                    bin :=''.
                    (char = 256) ifTrue:[bin := '000000001']
                        ifFalse:[
                            (char = 0) ifTrue:[bin := '000000000']
                                ifFalse:[
                                    8 to:1 by: -1 do:[:i|
                                        bin:=bin,((char bitAt:i)
                                        printString).
                                    ]
                                ].
                        ].
                    encString := (encString,'0',bin).
               ].
    ].
    dOpt ifTrue:[stdout << encString << nl].

    "Printing out the encoding table"
    tOpt ifTrue:[
        stdout <<nl<< 'Encoding table' << nl.
        encTable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
            stdout << key asCharacter visible 
                << '     '
                << (freqtable at: key) <<'    '<<value
                << nl.
            ].
        ].
    ].

    "Writing the bits to stdout or the outFile"
    (outFile ~= nil) ifTrue:[

        outBytes := OutBits new: outFile.
        1 to: (encString size) by: 1 do:[:index |
            bit := ((encString at: index) asInteger) - 48.
            outBytes writeBit: bit.
        ].

        outBytes writeBit: 1.

        [readFile atEnd not] whileTrue:[
            |ordChar|
            ordChar := readFile next asInteger.
            code := (encTable at: ordChar).
            1 to: (code size) by: 1 do: [:index |
                bit:= ((code at: index) asInteger) - 48.
                outBytes writeBit: bit.
            ].
        ].

        eof := (encTable at: 256).
            1 to: (eof size) by: 1 do: [:index |
                bit:= ((eof at: index) asInteger) - 48.
                outBytes writeBit: bit.
            ].
            outBytes flushByte.
    ]
    ifFalse:[
        dOpt ifTrue:[
        stdout <<nl<< 'optC = '<<cOpt << nl
              <<'no outputfile'<< nl.].
    
        
        stdout <<encString<<1 << nl.      
    
        [readFile atEnd not] whileTrue: [
           |ordChar|
           ordChar := readFile next asInteger.
           code:= (encTable at: ordChar).
           stdout <<code. 
        ].
        eof:= (encTable at: 256).
        stdout<<eof << nl. 
    ].  
]

"----------------------------------------"
"Decompression mode"
"----------------------------------------"
(uOpt) ifTrue:[
    |isEof outBytes stack|
    isEof := false.

    outBytes := OutBits new: outFile.
    stack := OrderedCollection new.
    inBits := BitStream new: inFile.
    treeBuilt := false.
    recTree :=''.
    root := ''.

    "Reads the bits from an input file"
    [(inBits atEnd not) & (isEof not)] whileTrue:[
        treeNode := inBits nextBit.
        leafC := 0.
        "Reconstructing the tree"
        treeBuilt ifFalse:[
            (treeNode = 0)ifTrue:[
                8 to: 1 by: -1 do:[:i|
                    bit := inBits nextBit.
                    leafC := leafC bitAt: i put: bit.
                ].
                (leafC = 0) ifTrue:[
                    padBit := inBits nextBit.
                    (padBit = 1) ifTrue:[
                        leafC := leafC bitAt: 9 put: 1.
                    ].
                ].
                stack add:(Leaf new: leafC count:'').
            ].

            (treeNode = 1)ifTrue:[
                (stack size > 1) ifTrue:[
                    rightc := stack removeLast.
                    leftc := stack removeLast.

                    newtree := (Tree new: -1
                                count: ''
                                left: leftc right: rightc).
                    stack add: newtree.
                ]
                ifFalse:[
                    treeBuilt := true.
                    recTree := stack removeLast.
                    root := recTree.
                ].
            ].
        ]
        ifTrue:[
            root isLeaf ifTrue:[
                ((root char) = 256) ifTrue:[
                    isEof := true.
                ]
                ifFalse:[
                    stdout nextPut: (Character value: (root char)).
                    root := recTree.
                ].
            ]
            ifFalse:[
                (treeNode = 0) ifTrue:[
                    root := root left.
                    (root isLeaf) ifTrue:[
                        (isEof not) ifTrue:[
                            ((root char) = 256) ifTrue:[
                                isEof := true.
                            ]
                            ifFalse:[
                                (outFile = nil)ifTrue:[
                                    stdout nextPut:(Character value: 
                                        (root char)).
                                ]
                                ifFalse:[
                                    8 to: 1 by: -1 do: [:index|
                                        bit := (root char) bitAt: index.
                                        outBytes writeBit: bit.
                                    ].
                                ].
                                root := recTree.
                            ].
                        ].
                    ]
                ]
                ifFalse:[
                    root := root right.
                    root isLeaf ifTrue:[
                        (isEof not) ifTrue:[
                            ((root char) = 256) ifTrue:[
                                isEof := true.
                            ]
                            ifFalse:[
                                (outFile = nil)ifTrue:[
                                    stdout nextPut:(Character value: 
                                        (root char)).
                                ]
                                ifFalse:[
                                    8 to: 1 by: -1 do: [:index|
                                        bit := (root char) bitAt: index.
                                        outBytes writeBit: bit.
                                    ].
                                ].
                                root := recTree.
                            ].
                        ].
                    ].
                ].
            ].
        ].
    ].
].

inFile close.
(outFile~=nil) ifTrue:[
outFile close.
].
