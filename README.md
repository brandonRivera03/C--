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
