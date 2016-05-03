# Program Name: C--

------------------------------------------------
Hey I uploaded a src, you can extract it and replace the src file
you'd normally use for eclipse and it should run fine. All it is
is the lolcode and its demo with keywords replaced with something more
natural. 

#04/25/2016:
I uploaded an updated Cminusminus.scala and cmmTest.scala. Replace them as usual.
Two major changes: loops now require a conditional, just as a normal while loop does. To break out of the loop one must either use a break statement or the conditional must fail at some point.
I also changed how functions appear to be more sentence-like. For example, an if statement would follow the lines of:
        If ('this isLessThan 'that)
Basically uses actually words and things like Print and If and stuff are capitalized. So it isn't closer to IO programming language, but more original I suppose. We don't need to follow IO exactly I guess, just say we made up a new language ourselves.

I know I said I would change if statements to be better but in actuality it works just fine originally, all the basic functionality is there, other than an elseif statement.

#04/28/2016:
Fixed an error I made for if and while: before it didn't allow for nested if or while loops, but now it should work.
Also added scoping for if and while loops. Originally a declaration made in an if statement could carry over outside, now it can't, which is more correct. 

I also added more functionality for them: conditionals can now take straight bool values, functions, or variables. Before it could only do something like while(true), if(this is less than that), and if(variable).

Added and & or operators, function as normal bool operators.

I was trying to add an increment operator, so you could call Increment ('variable) by (10) or something, but it wasn't working. Maybe someone else can try that.

#04/29/2016
Ok so normal assignments are done by first identifying the object Variable, which takes in a symbol (uses notation like: 'x) and then calls Assignment class on (s). The assignment class has an s and expects a value v, v being a string, an int, a double, or a function. It uses the keywords "is" or "equals" to allow the next step. The assignment class calls class Assign, which requires a line number as well as a function, that function in this case binds the symbol and value onto the bindings hashmap. So in order: Create a variable symbol, give it a value with type (int, double, string, or function that returns type any, which is all of the above), and then bind it to the hashmap of the particular scope.

To implement increment, I tried to add in an alternative to assignment. It would start with an object Increment, which given a symbol would call a variation of the Assignment class, which would use a keyword "by" instead of "is" or "equals". Then I would try to get the value already stored by symbol and increment it by the new value. So it would be like:

Increment ('number) by (1).

Unfortunately Scala throws an error saying that the symbol doesn't exist on the hashmap, even if it was clearly instatiated earlier. So the challenge is trying to get the value stored earlier now and solving why it doesn't seem to be on the hashmap.

As for lists, you would need to add a type List in addition to the types Any and Anyval, etc that are already there. Ideally you would use a different assignment object once again: so instead of Variable, you would make an object List, which calls some other modified Assignment class and uses its own keywords, etc. So perhaps a list would be instantiated like:

List ('x) includes (1, 2, 3, 4, 5)

#05/02/2016
line 51, in def End():
gotoLine(lines.keys.toList.sorted.head)
occassionally (maybe 1 in 5 times) will jump to some line it shouldn't really go to. This is the call stack I was getting:

Exception in thread "main" java.util.NoSuchElementException: head of empty list
	at scala.collection.immutable.Nil$.head(List.scala:420)
	at scala.collection.immutable.Nil$.head(List.scala:417)
	at scala.collection.mutable.Stack.top(Stack.scala:132)
	at cmm.Cminusminus$Bindings.getMap(Cminusminus.scala:1349)
	at cmm.Cminusminus$Bindings.any(Cminusminus.scala:1395)
	at cmm.Cminusminus$$anonfun$gotoLine$1.apply(Cminusminus.scala:218)
	at scala.collection.TraversableLike$$anonfun$map$1.apply(TraversableLike.scala:234)
	at scala.collection.TraversableLike$$anonfun$map$1.apply(TraversableLike.scala:234)
	at scala.collection.Iterator$class.foreach(Iterator.scala:893)
	at scala.collection.AbstractIterator.foreach(Iterator.scala:1336)
	at scala.collection.IterableLike$class.foreach(IterableLike.scala:72)
	at scala.collection.AbstractIterable.foreach(Iterable.scala:54)
	at scala.collection.TraversableLike$class.map(TraversableLike.scala:234)
	at scala.collection.AbstractTraversable.map(Traversable.scala:104)
	at cmm.Cminusminus.gotoLine(Cminusminus.scala:217)
	at cmm.Cminusminus.GeneralWhile$1(Cminusminus.scala:144)
	at cmm.Cminusminus.gotoLine(Cminusminus.scala:295)
	at cmm.Cminusminus.End(Cminusminus.scala:52)
	at cmm.cmmTest$.main(cmmTest.scala:31)
	at cmm.cmmTest.main(cmmTest.scala)

After some testing it appears to happen due to the random number generator so we may have to avoid using that or just get rid of it altogether

Also i dunno how to propery adjust how github aligns tabs and stuff so the error message just looks like garbage, sorry about that but idk

------------------------------------------------
Make it more easier for a programmer to type/ program
Adding "Syntactic sugar"

Example:
val x = 10;
Variable x is 10.

* Create flexibility --> Assign and Define mean same thing


Datatypes: Int, Bool, Float, "String", List

Create List:
The variable L is a list of <Type>
"    "       " "  " <Type> list that contains <values>

The first element of L is <statement>

Access first element of L
Add <variable> to L

        - Zero indexing?
------------------------------------------------

; == end of line
. == end of program

* variables are fixed/ static
* for loops - state iteration by number not user variable

for loop := Repeat [Int] times

x += num := Reassign x to be x plus num
            Add num to x
x = x/3 --> return integer division

Possible functions:
Square root
Plus, Add (Minus, Subtract)
Write, Assign

================================================
Concept: Display
        Problematic approaches:
                - Display "string" ;
                - Display object .
                - Display object \n
        Desired approach:
                * Display (string).

Concept: Print a more fixed/ specific of Display
===================================================

Conditionals:
If <Bool>, then <statement 1>, else/otherwise <statement 2> ;

Loops:
Repeat <statement> <Int> times                  // for loop
While <conditional> is True, Do <statement>     // while loops


Functions:
The <function_name> of/uses <arg1, arg2, ...> and returns <Type>
// place functions list


Return statements:
Return <statment>.
======================================================

Typecasting:

Doubles subset of Int, so convert Int into Double
1 -> 1.0

========================================================
========================================================

Tasks:
1) Work on Parser
