package cmm

import scala.collection.mutable.{ HashMap, Stack }
import scala.util.Random
import scala.math.{ min, max }
import scala.io.StdIn.{ readLine }

class Cminusminus {
  abstract sealed class C_Line
  case class PrintString(num: Int, s: String) extends C_Line
  case class PrintVariable(num: Int, s: Symbol) extends C_Line
  case class PrintNumber(num: Int, s: Int) extends C_Line
  case class PrintFunction(num: Int, s: Function0[Any]) extends C_Line
  case class PrintMany(num: Int, s: Vector[Any]) extends C_Line
  case class PrintArray(num: Int, s: Array[Any]) extends C_Line
  case class ReadString(num: Int, s: Symbol) extends C_Line
  case class ErrorPrintString(num: Int, s: String) extends C_Line
  case class ErrorPrintVariable(num: Int, s: Symbol) extends C_Line
  case class ErrorPrintNumber(num: Int, s: Int) extends C_Line
  case class ErrorPrintFunction(num: Int, s: Function0[Any]) extends C_Line
  case class ErrorPrintMany(num: Int, s: Vector[Any]) extends C_Line
  case class IfStatement(num: Int, fun: Function0[Boolean]) extends C_Line
  case class IfSymb(num: Int, sym: Symbol) extends C_Line
  case class IfNormal(num: Int, s: Boolean) extends C_Line
  case class ElseStatement(num: Int) extends C_Line
  case class EndIfStatement(num: Int) extends C_Line
  case class Assign(num: Int, fn: Function0[Unit]) extends C_Line
  case class LoopBegStatement(num: Int, fun: Function0[Boolean]) extends C_Line
  case class LoopBegSymb(num: Int, sym: Symbol) extends C_Line
  case class LoopBegNormal(num: Int, bool: Boolean) extends C_Line
  case class BreakLoop() extends C_Line
  case class LoopEnd(loopBegLine: Int) extends C_Line
  case class FuncBeg(name: Symbol) extends C_Line
  case class FuncEnd() extends C_Line
  case class FuncReturn(value: Any) extends C_Line
  case class FuncCall(funcName: Symbol) extends C_Line
  case class FuncCallReturn(funcName: Symbol, variable: Symbol) extends C_Line
  case class EndStatement(num: Int) extends C_Line

  /* Current line. */
  var current: Int = 1

  var lines = new HashMap[Int, C_Line]
  val binds = new Bindings
  val funcBegLines = new HashMap[Symbol, Int]
  val random = new Random
  val loopBegLines = new Stack[Int]
  val pcStack = new Stack[Int]
  val returnStack = new Stack[Any]

  def End() = {
    lines(current) = EndStatement(current)
    gotoLine(lines.keys.toList.sorted.head)
  }

  def Begin() = {
    lines = new HashMap[Int, C_Line]
    binds.newScope()
  }

  def Else() = {
    lines(current) = ElseStatement(current)
    current += 1
  }

  def EndIf() = {
    lines(current) = EndIfStatement(current)
    current += 1
  }

  /* General Assignment functions. */
  case class Assignment(sym: Symbol) {
    def is(v: String): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def is(v: AnyVal): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def is(v: List[Any]): Unit = {
      lines(current) = Assign(current, (() => binds.setList(sym,v)))
      current += 1
    }
    def is(v: Function0[Any]): Unit = {
      val val_v = v()
      val_v match {
        case x: List[Any] => lines(current) = Assign(current, (() => binds.setList(sym, v().asInstanceOf[List[Any]])))
        case x: Int => lines(current) = Assign(current, (() => binds.set(sym, v().asInstanceOf[Int])))
        case x: Double => lines(current) = Assign(current, (() => binds.set(sym, v().asInstanceOf[Double])))
        case x: Array[Any] => lines(current) = Assign(current, (() => binds.set(sym, v().asInstanceOf[Array[Any]])))
        case _ => lines(current) = Assign(current, (() => binds.set(sym, v())))
      }
      current += 1
    }
    def is(v: Array[String]): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def is(v: Symbol): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, binds.any(v))))
      current += 1
    }

    def equals(v: String): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def equals(v: AnyVal): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def equals(v: List[Any]): Unit = {
      lines(current) = Assign(current, (() => binds.setList(sym,v)))
      current += 1
    }
    def equals(v: Function0[Any]): Unit = {
      val val_v = v()
      val_v match {
        case x: List[Any] => lines(current) = Assign(current, (() => binds.setList(sym, v().asInstanceOf[List[Any]])))
        case x: Int => lines(current) = Assign(current, (() => binds.set(sym, v().asInstanceOf[Int])))
        case x: Double => lines(current) = Assign(current, (() => binds.set(sym, v().asInstanceOf[Double])))
        case x: Array[Any] => lines(current) = Assign(current, (() => binds.set(sym, v().asInstanceOf[Array[Any]])))
        case _ => lines(current) = Assign(current, (() => binds.set(sym, v())))
      }
      current += 1
    }
    def equals(v: Array[String]): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def equals(v: Symbol): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, binds.any(v))))
      current += 1
    }
    
    def isDefinedAs(v: String): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def isDefinedAs(v: AnyVal): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def isDefinedAs(v: List[Any]): Unit = {
      lines(current) = Assign(current, (() => binds.setList(sym,v)))
      current += 1
    }
    def isDefinedAs(v: Function0[Any]): Unit = {
      val val_v = v()
      val_v match {
        case x: List[Any] => lines(current) = Assign(current, (() => binds.setList(sym, v().asInstanceOf[List[Any]])))
        case x: Int => lines(current) = Assign(current, (() => binds.set(sym, v().asInstanceOf[Int])))
        case x: Double => lines(current) = Assign(current, (() => binds.set(sym, v().asInstanceOf[Double])))
        case x: Array[Any] => lines(current) = Assign(current, (() => binds.set(sym, v().asInstanceOf[Array[Any]])))
        case _ => lines(current) = Assign(current, (() => binds.set(sym, v())))
      }
      current += 1
    }
    def isDefinedAs(v: Array[String]): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def isDefinedAs(v: Symbol): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, binds.any(v))))
      current += 1
    }
  }
  

  /* Runtime Evaluation. */
  private def gotoLine(line: Int) {

    def GeneralWhile(bool: Boolean): Unit = {
      /* If the conditional was true. */
      if (bool) {
        binds.newScope()
        gotoLine(line + 1)
      }
      /* Go to end of loop. */
      else {
        var curLine = line
        var advance = true
        var cnt = 1
        while (advance) {
          curLine += 1
          if (lines(curLine).isInstanceOf[LoopEnd]) {
            cnt -= 1
            if (cnt == 0)
              advance = false
          }
          else if (lines(curLine).isInstanceOf[LoopBegStatement]
                  || lines(curLine).isInstanceOf[LoopBegSymb]
                  || lines(curLine).isInstanceOf[LoopBegNormal]) {
            cnt += 1
          }
        }
        gotoLine(curLine + 1)
      }
    }
    
    def GeneralIf(bool: Boolean): Unit = {
      /* If the conditional was true. */
      if (bool) {  
        binds.newScope()
        gotoLine(line + 1)
      } 
      /* Find next statement we need to run, if any. */
      else {       
        var curLine = line
        var advance = true
        var cnt = 0  //Keep track of nested if statements
        while (advance) {
          curLine += 1
          if (lines(curLine).isInstanceOf[EndIfStatement]) {
            cnt -= 1
            if (cnt < 0)//Holds if there are no other nested else statements
              advance = false
          } 
          else if (lines(curLine).isInstanceOf[ElseStatement]) {
            if (cnt == 0)//Holds if there are no nested else statements
              advance = false
          }
          else if (lines(curLine).isInstanceOf[IfStatement]
                  || lines(curLine).isInstanceOf[IfSymb]
                  || lines(curLine).isInstanceOf[IfNormal]) {
            cnt += 1
          }
        }
        gotoLine(curLine + 1)
      }
    }

    lines(line) match {
      case PrintString(_, s: String) => {
        println(s)
        gotoLine(line + 1)
      }
      
      
      case PrintVariable(_, s: Symbol) => {
        
        var map = binds.getMap(s)
        if (map.x.contains(s)){
                   try{
          val array = binds.arrayval(s)
          println(array.mkString(" "))  
         } catch{
           case e: Exception =>{
             println(binds.any(s))
           }
         } 
        }
        else if (map.y.contains(s)){
          println(binds.anyList(s))
        }
        gotoLine(line + 1)
      }   
        

      case PrintNumber(_, s: Int) => {
        println(s)
        gotoLine(line + 1)
      }
      case PrintFunction(_, s: Function0[Any]) => {
        println(s())
        gotoLine(line + 1)
      }
      case PrintMany(_, s: Vector[Any]) => {

        println(s.map(e => e match {
          case v: Symbol => binds.any(v)
          case v: Function0[Any] => v()
          case _ => e
        }).mkString(" "))

        gotoLine(line + 1)
      }
      
      case PrintArray(_,s: Array[Any]) =>{
        println(s.mkString(" "))
        gotoLine(line+1)
      }

      case ErrorPrintString(_, s: String) => {
        Console.err.println(Console.RED + s + Console.RESET)
        gotoLine(line + 1)
      }
      case ErrorPrintVariable(_, s: Symbol) => {
        Console.err.println(Console.RED + binds.any(s) + Console.RESET)
        gotoLine(line + 1)
      }
      case ErrorPrintNumber(_, s: Int) => {
        Console.err.println(Console.RED + s + Console.RESET)
        gotoLine(line + 1)
      }
      case ErrorPrintFunction(_, s: Function0[Any]) => {
        Console.err.println(Console.RED + s() + Console.RESET)
        gotoLine(line + 1)
      }
      case ErrorPrintMany(_, s: Vector[Any]) => {

        Console.err.println(Console.RED + s.map(e => e match {
          case v: Symbol => binds.any(v)
          case v: Function0[Any] => v()
          case _ => e
        }).mkString(" ") + Console.RESET)

        gotoLine(line + 1)
      }

      case ReadString(_, s: Symbol) => {
        val value: Any = tryInt(readLine())
        println(value)
        binds.set(s, value)
        gotoLine(line + 1)
      }

      case IfStatement(_, fun: Function0[Boolean]) => {
        GeneralIf(fun())
      }

      case IfSymb(_, sym: Symbol) => {
        GeneralIf(binds.any(sym).asInstanceOf[Boolean])
      }
      
      case IfNormal(_, bool: Boolean) => {
        GeneralIf(bool)
      }
      case ElseStatement(_) => {
        var curLine = line
        while (!lines(curLine).isInstanceOf[EndIfStatement]) {
          curLine = curLine + 1;
        }
        gotoLine(curLine + 1);
      }

      case EndIfStatement(_) => {
        binds.leaveScope()
        gotoLine(line + 1);
      }

      case Assign(_, fn: Function0[Unit]) =>
        {
          fn()
          gotoLine(line + 1)
        }
      
      case LoopBegStatement(_, fun: Function0[Boolean]) => {
        GeneralWhile(fun())
      }
      
      case LoopBegSymb(_, sym: Symbol) => {
        GeneralWhile(binds.any(sym).asInstanceOf[Boolean])
      }
      case LoopBegNormal(_, bool: Boolean) => {
        GeneralWhile(bool)
      }
      
      /* Find the corresponding LoopEnd instance. */
      case BreakLoop() => {
        binds.leaveScope()
        var curLine = line
        var advance = true
        var cnt = 1
        while (advance) {
          curLine += 1
          if (lines(curLine).isInstanceOf[LoopEnd]) {
            cnt -= 1
            if (cnt == 0)
              advance = false
          }
          else if (lines(curLine).isInstanceOf[LoopBegStatement]
                  || lines(curLine).isInstanceOf[LoopBegSymb]
                  || lines(curLine).isInstanceOf[LoopBegNormal]) {
            cnt += 1
          }
        }
        gotoLine(curLine + 1)
      }
      
      /* Loop back to LoopBeg. */
      case LoopEnd(loopBegLine: Int) => {
        gotoLine(loopBegLine)
      }
      
      case FuncBeg(name: Symbol) => {
        var lineVar = line
        while (!lines(lineVar).isInstanceOf[FuncEnd]) {
          lineVar += 1
        }
        gotoLine(lineVar + 1)
      }
      
      case FuncEnd() => {

        val temp: Any = returnStack.pop()

        binds.leaveScope()

        temp match {
          case t: Function0[Any] => {
            if (returnStack.length > 0) {
              returnStack.pop() match {
                case v: Symbol => {
                  binds.set(v, t())
                }
                case v => {
                  v match {
                    case None => 
                    case _ => {
                      returnStack.push(v)
                      returnStack.push(t)
                    }
                  }
                }
              }
            }
          }
          case t: Int => {
            if (returnStack.length > 0) {
              returnStack.pop() match {
                case v: Symbol => {
                  binds.set(v, t)
                }
                case v => {
                  v match {
                    case None =>
                    case _ => {
                      returnStack.push(v)
                      returnStack.push(t)
                    }
                  }
                }
              }
            }
          }
          case t: Symbol => {
            returnStack.push(t)
          }
          case None =>
          case _ => throw new RuntimeException(f"Something bad has happened! $temp")
        }

        gotoLine(pcStack.pop())
      }
      
      /* Update location but ignore for now. */
      case FuncReturn(value: Any) => {
        value match {
          case v: Function0[Any] => returnStack.push(v())
          case v: Symbol => returnStack.push(binds.any(v))
          case v => returnStack.push(v)
        }
        var lineVar = line
        while (!lines(lineVar).isInstanceOf[FuncEnd]) {
          lineVar += 1
        }
        gotoLine(lineVar)
      }
      
      /* Update location we want to return to and go to where FuncReturn is. */
      case FuncCall(funcName: Symbol) => {
        returnStack.push(None)
        pcStack.push(line + 1)
        binds.newScope()
        gotoLine(funcBegLines.get(funcName) match {
          case Some(s) => s + 1
          case None => -1
        })
      }
      
      case FuncCallReturn(funcName: Symbol, variable: Symbol) => {
        returnStack.push(variable)
        pcStack.push(line + 1)
        binds.newScope()
        gotoLine(funcBegLines.get(funcName) match {
          case Some(s) => s + 1
          case None => -1
        })
      }
      
      case EndStatement(_) =>
      case _ =>
    }
  }

  /* Random number generator functions. */
  //It seems that calling this repeatedly during run time always returns the same value
  def between(i: Int, j: Int): Int = { 
    random.nextInt(j + 1 - i) + i 
  }
  def eitherTrueOrFalse: Boolean = {
    random.nextBoolean
  }
  
  /* Maximum and Minimum functions. */
  def theHighestValueBetween(i: Any, j: Any): Function0[Any] = {
    () =>
      {
        val base_i = i match {
          case _i: Symbol => binds.anyval(_i)
          case _i: Function0[Any] => _i()
          case _ => i
        }

        val base_j = j match {
          case _j: Symbol => binds.anyval(_j)
          case _j: Function0[Any] => _j()
          case _ => j
        }

        base_i match {
          case _i: Int => {
            base_j match {
              case _j: Int => max(_i, _j)
              case _j: Double => max(_i, _j)
            }
          }
          case _i: Double => {
            base_j match {
              case _j: Int => max(_i, _j)
              case _j: Double => max(_i, _j)
            }
          }
        }
      }
  }

  def theLowestValueBetween(i: Any, j: Any): Function0[Any] = {
    () =>
      {
        val base_i = i match {
          case _i: Symbol => binds.anyval(_i)
          case _i: Function0[Any] => _i()
          case _ => i
        }

        val base_j = j match {
          case _j: Symbol => binds.anyval(_j)
          case _j: Function0[Any] => _j()
          case _ => j
        }

        base_i match {
          case _i: Int => {
            base_j match {
              case _j: Int => min(_i, _j)
              case _j: Double => min(_i, _j)
            }
          }
          case _i: Double => {
            base_j match {
              case _j: Int => min(_i, _j)
              case _j: Double => min(_i, _j)
            }
          }
        }
      }
  }

  /* Returns the List object */
  def get(sym : Symbol) : Function0[Any] = {
    () => {
      val x = binds.anyList(sym)
      x 
    }
  }

  /*
   * Returns a List consisting of the tail, or a list without the
   * head element
   */
  def getTail(sym : Symbol) : Function0[Any] = {
    () => {
      var x : List[Any] = binds.anyList(sym)     
      
      if(x.size > 0) {
        x tail
      }
      else {
        None
      }
    }
  }
  def theLastValueOf(sym : Symbol) : Function0[Any] = {
    () => {
      var x : List[Any] = binds.anyList(sym)     
      
      if(x.size > 0) {
        x tail
      }
      else {
        None
      }
    }
  }

  /* Returns the head element of a list */
  def getHead(sym : Symbol) : Function0[Any] = {
    () => {
      var x : List[Any] = binds.anyList(sym)

      if(x.size > 0) {
        x head
      }
      else {
        None
      }
    }
  }
  def theFirstValueOf(sym : Symbol) : Function0[Any] = {
    () => {
      var x : List[Any] = binds.anyList(sym)

      if(x.size > 0) {
        x head
      }
      else {
        None
      }
    }
  }
  /* Returns the element associated to an index of a list */
  def get(sym : Symbol, idx : Any) : Function0[Any] = {
    () => {
      var x : List[Any] = binds.anyList(sym)

      /* Type check and convert the value to an Int, if applicable */
      if(x.size != 0) {
        val index = idx match {
          case _i: Int => _i
          case _i: Double => _i.asInstanceOf[Int]
          case _i: Symbol => (binds.anyval(_i)).asInstanceOf[Int]
          case _i: Function0[Any] => _i().asInstanceOf[Int]
          case _ => throw new RuntimeException(f"Error: Cannot retrieve element of $sym because $idx is not a valid index")
        }

      /* In case the user puts an index outside the size of the list
       * then we take the modulo of it */
        val i = index % x.size

        x(i)
      }
      else {
        None
      }
    }
  }

  /* Returns a list with a value inserted into an existing list
   * at a specified index
   */
  def insert(sym : Symbol, idx : Any, v : Any) : Function0[Any] = {
    () => {
      var x: List[Any] = binds.anyList(sym)

      /* Type check and convert the value to an Int, if applicable */
      if(x.size != 0) {
        val index = idx match {
          case _i: Int => _i
          case _i: Double => _i.asInstanceOf[Int]
          case _i: Symbol => (binds.anyval(_i)).asInstanceOf[Int]
          case _i: Function0[Any] => _i().asInstanceOf[Int]
          case _ => throw new RuntimeException(f"Error: Cannot retrieve element of $sym because $idx is not a valid index")
        }

      /* In case the user puts an index outside the size of the list
       * then we take the modulo of it */
        val i = index % x.size

        (x.slice(0, i) :+ v) ::: x.slice(i, x.size)
      }
      else {
        x :+ v
      }
    }
  }

  /* Returns a list with that updates a value to a specified index
   */
  def update(sym : Symbol, idx : Any, v : Any) : Function0[Any] = {
    () => {
      var x: List[Any] = binds.anyList(sym)

      /* Type check and convert the value to an Int, if applicable */
      if(x.size != 0) {
        val index = idx match {
          case _i: Int => _i
          case _i: Double => _i.asInstanceOf[Int]
          case _i: Symbol => (binds.anyval(_i)).asInstanceOf[Int]
          case _i: Function0[Any] => _i().asInstanceOf[Int]
          case _ => throw new RuntimeException(f"Error: Cannot retrieve element of $sym because $idx is not a valid index")
        }

      /* In case the user puts an index outside the size of the list
       * then we take the modulo of it */
        val i = index % x.size

        val ret = (x.slice(0, i) :+ v) ::: x.slice(i+1, x.size)
        ret
      }
      else {
        x :+ v
      }
    }
  } 

  /* Returns a list with a value or list appended to the end of another
   * list
   */
  def append(sym : Symbol, v : Any) : Function0[Any] = {
    () => {
      var x: List[Any] = binds.anyList(sym)

      v match {
        case l: List[Any] => x ::: l
        case _ => x :+ v
      }
    }
  }

  /* Returns a list with a value or list appended to the beginning of
   * another list 
   */
  def prepend(sym : Symbol, v : Any) : Function0[Any] = {
     () => {
      var x: List[Any] = binds.anyList(sym)

      v match {
        case l: List[Any] => l ::: x
        case _ => v +: x
      }
     }
  }

  /* Returns the size of a list */
  def getSize(sym : Symbol) : Function0[Any] = {
    () => {
      var x: List[Any] = binds.anyList(sym)

      if(x.size != 0) {
        x.size
      }
      else {
        0
      }
    }
  }
  def theSizeOf(sym : Symbol) : Function0[Any] = {
    () => {
      var x: List[Any] = binds.anyList(sym)

      if(x.size != 0) {
        x.size
      }
      else {
        0
      }
    }
  }
  def theNumberOfElementsIn(sym : Symbol) : Function0[Any] = {
    () => {
      var x: List[Any] = binds.anyList(sym)

      if(x.size != 0) {
        x.size
      }
      else {
        0
      }
    }
  }
  
  /* Returns a list that removes a single element from another list */
  def remove(sym : Symbol, idx : Any) : Function0[Any] = {
    () => {
      var x: List[Any] = binds.anyList(sym)
      var i = 0

      /* Type check and convert the value to an Int, if applicable */
      if(x.size != 0) {
        val index = idx match {
          case _i: Int => _i
          case _i: Double => _i.asInstanceOf[Int]
          case _i: Symbol => (binds.anyval(_i)).asInstanceOf[Int]
          case _i: Function0[Any] => _i().asInstanceOf[Int]
          case _ => throw new RuntimeException(f"Error: Cannot retrieve element of $sym because $idx is not a valid index")
        }

      /* In case the user puts an index outside the size of the list
       * then we take the modulo of it */
        val i = index % x.size

        x.slice(0, i) ::: x.slice(i+1, x.size)
      }
      else {
        x
      }
    }
  }

  /* Returns a list that removes a number of elements from one index
   * to another index from a specified list
   */
  def remove(sym : Symbol, idx1 : Any, idx2 : Any) : Function0[Any] = {
    () => {
      var x: List[Any] = binds.anyList(sym)

      /* Type check and convert the value to an Int, if applicable */
      if(x.size != 0) {
        val index1 = idx1 match {
          case _i: Int => _i
          case _i: Double => _i.asInstanceOf[Int]
          case _i: Symbol => (binds.anyval(_i)).asInstanceOf[Int]
          case _i: Function0[Any] => _i().asInstanceOf[Int]
          case _ => throw new RuntimeException(f"Error: Cannot retrieve element of $sym because $idx1 is not a valid index")
        }
        
        val index2 = idx2 match {
          case _i: Int => _i
          case _i: Double => _i.asInstanceOf[Int]
          case _i: Symbol => (binds.anyval(_i)).asInstanceOf[Int]
          case _i: Function0[Any] => _i().asInstanceOf[Int]
          case _ => throw new RuntimeException(f"Error: Cannot retrieve element of $sym because $idx1 is not a valid index")
        }

      /* In case the user puts an index outside the size of the list
       * then we take the modulo of it */
        val i = index1 % x.size
        val j = index2 % x.size

        val minIdx = min(i, j)
        val maxIdx = max(i, j)

        x.slice(0, minIdx) ::: x.slice(maxIdx, x.size)
      }
      else {
        x
      }
    }
  }

  /* General Operations. */
  implicit def operator_any(i: Any) = new {
    
    /* Array indexing. */
    def index(j: Any):Function0[Any] = {
       () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.arrayval(_i)
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.num(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Array[Any] => {
              base_j match {
                case _j: Int => _i(_j)
              }
            }
          }
        }
    }
    
    def atIndex(j: Any):Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.arrayval(_i)
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.num(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Array[Any] => {
              base_j match {
                case _j: Int => _i(_j)
              }
            }
          }
        }
    }
    
    def pos(j: Any):Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.arrayval(_i)
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.num(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Array[Any] => {
              base_j match {
                case _j: Int => _i(_j)
              }
            }
          }
        }
    }
    
    def atPosition(j: Any):Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.arrayval(_i)
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.num(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Array[Any] => {
              base_j match {
                case _j: Int => _i(_j)
              }
            }
          }
        }
    }
    /* Basic Mathematical Operations. */
    def plus(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i + _j
                case _j: Double => _i + _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i + _j
                case _j: Double => _i + _j
              }
            }
          }
        }
    }

    def minus(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i - _j
                case _j: Double => _i - _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i - _j
                case _j: Double => _i - _j
              }
            }
          }
        }
    }

    def times(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i * _j
                case _j: Double => _i * _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i * _j
                case _j: Double => _i * _j
              }
            }
          }
        }
    }

    def dividedBy(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i / _j
                case _j: Double => _i / _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i / _j
                case _j: Double => _i / _j
              }
            }
          }
        }
    }

    def modulo(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i % _j
                case _j: Double => _i % _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i % _j
                case _j: Double => _i % _j
              }
            }
          }
        }
    }
    
    /* Basic Boolean Operations. */  
    def and(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.any(_i).asInstanceOf[Boolean]
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.any(_j).asInstanceOf[Boolean]
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Boolean => {
              base_j match {
                case _j: Boolean => _i && _j
              }
            }
          }
        }
    }
    
    def or(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.any(_i).asInstanceOf[Boolean]
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.any(_j).asInstanceOf[Boolean]
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Boolean => {
              base_j match {
                case _j: Boolean => _i || _j
              }
            }
          }
        }
    }
    
    def isGreaterThan(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i > _j
                case _j: Double => _i > _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i > _j
                case _j: Double => _i > _j
              }
            }
          }
        }
    }

    def isLessThan(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i < _j
                case _j: Double => _i < _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i < _j
                case _j: Double => _i < _j
              }
            }
          }
        }
    }

    def isEqualTo(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i == _j
                case _j: Double => _i == _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i == _j
                case _j: Double => _i == _j
              }
            }
          }
        }
    }

    def isEquivalentTo(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.any(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.any(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          val new_i = base_i match {
            case _i: Int => _i
            case _i: Double => _i.asInstanceOf[Int]
            case _i: Function0[Any] => _i()
            case bool if bool == true => 1
            case bool if bool == false => 0
            case _ => base_i
          }
          
          val new_j = base_j match {
            case _j: Int => _j
            case _j: Double => _j.asInstanceOf[Int]
            case _j: Function0[Any] => _j()
            case bool if bool == true => 1
            case bool if bool == false => 0
            case _ => base_j
          }
          new_i == new_j
        }
    }
    
    def isNotEquivalentTo(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.any(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.any(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          val new_i = base_i match {
            case _i: Int => _i
            case _i: Double => _i.asInstanceOf[Int]
            case _i: Function0[Any] => _i()
            case bool if bool == true => 1
            case bool if bool == false => 0
            case _ => base_i
          }
          
          val new_j = base_j match {
            case _j: Int => _j
            case _j: Double => _j.asInstanceOf[Int]
            case _j: Function0[Any] => _j()
            case bool if bool == true => 1
            case bool if bool == false => 0
            case _ => base_j
          }
          
          new_i != new_j
        }
    }    

    def isNot(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i != _j
                case _j: Double => _i != _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i != _j
                case _j: Double => _i != _j
              }
            }
          }
        }
    }
  }

  /* Read from keyboard. */
  object Input {
    def apply(s: Symbol) = {
      lines(current) = ReadString(current, s)
      current += 1
    }
  }

  def tryInt(s: String): Any = {
    try {
      s.toInt
    } catch {
      case e: Exception => s
    }
  }

  object Print {
    def apply(s: String) = {
      lines(current) = PrintString(current, s)
      current += 1
    }
    def apply(s: Array[Any]) ={
      lines(current) = PrintArray(current, s)
      current += 1
      
    }
    def apply(s: Any*) = {
      lines(current) = PrintMany(current, s.toVector)
      current += 1  
    }
    def apply(s: Symbol) = {
      lines(current) = PrintVariable(current, s)
      current += 1
    }
    def apply(s: Int) = {
      lines(current) = PrintNumber(current, s)
      current += 1
    }
    def apply(s: Function0[Any]) = {
      lines(current) = PrintFunction(current, s)
      current += 1
    }
  }

  object Error {
    def apply(s: String) = {
      lines(current) = ErrorPrintString(current, s)
      current += 1
    }
    def apply(s: Any*) = {
      lines(current) = ErrorPrintMany(current, s.toVector)
      current += 1
    }
    def apply(s: Symbol) = {
      lines(current) = ErrorPrintVariable(current, s)
      current += 1
    }
    def apply(s: Int) = {
      lines(current) = ErrorPrintNumber(current, s)
      current += 1
    }
    def apply(s: Function0[Any]) = {
      lines(current) = ErrorPrintFunction(current, s)
      current += 1
    }
  }

  object Variable {
    def apply(s: Symbol) = Assignment(s)
  }
  
  
  object EmptyVariable {
    def apply(s: Any) = {}
  }

  object If {
    def apply(s: Function0[Boolean]) = {
      lines(current) = IfStatement(current, s)
      current += 1
    }
    def apply(s: Symbol) = {
      lines(current) = IfSymb(current, s)
      current += 1
    }
    def apply(s: Boolean) = {
      lines(current) = IfNormal(current, s)
      current += 1
    }
  }
  object While {
    def apply(s: Function0[Boolean]) = {
      lines(current) = LoopBegStatement(current, s)
      loopBegLines.push(current)
      current += 1
    }
    def apply(s: Symbol) {
      lines(current) = LoopBegSymb(current, s)
      loopBegLines.push(current)
      current += 1
    }
    def apply(s: Boolean) {
      lines(current) = LoopBegNormal(current, s)
      loopBegLines.push(current)
      current += 1
    }
  }


  def Done {
    lines(current) = LoopEnd(loopBegLines.pop())
    current += 1
  }

  def Break {
    lines(current) = BreakLoop()
    current += 1
  }

  object Function {
    def apply(funcName: Symbol) = {
      funcBegLines += (funcName -> current)
      lines(current) = FuncBeg(funcName)
      current += 1
    }
  }

  object Return {
    def apply(value: Any) = {
      lines(current) = FuncReturn(value)
      current += 1
    }
  }

  def EndFunction{
    lines(current) = FuncEnd()
    current += 1
  }

  object Call {
    def apply(funcName: Symbol) = {
      lines(current) = FuncCall(funcName)
      current += 1
    }
    def apply(funcName: Symbol, variable: Symbol) = {
      lines(current) = FuncCallReturn(funcName, variable)
      current += 1
    }
  }

  /* To allow Lists, we must make a different hashmap separate from normal vals. */
  class Tables(){
    var x = HashMap[Symbol, Any]()
    var y = HashMap[Symbol, List[Any]]()
  }
  
  /* The hashmap where symbols are binded with values. */
  class Bindings {
    val bindingsStack = Stack[Tables]()
    val bindings = HashMap[Symbol, Any]()

    /* Creates new topmost scope. */
    def newScope() {
      bindingsStack.push(new Tables)
    }

    /* Destroys topmost scope. */
    def leaveScope() {
      bindingsStack.pop()
    }

    /* Get map based on scope. */
    def getMap(sym: Symbol): Tables = {
      val bindingsStackCopy = Stack[Tables]()
      val bindingsStackTop = bindingsStack.top
      while (!bindingsStack.isEmpty && !(bindingsStack.top.x.contains(sym) || bindingsStack.top.y.contains(sym))) {
        bindingsStackCopy.push(bindingsStack.pop())
      }
      var map = bindingsStackTop
      if (!bindingsStack.isEmpty) {
        map = bindingsStack.top
      }
      while (!bindingsStackCopy.isEmpty) {
        bindingsStack.push(bindingsStackCopy.pop())
      }
      map
    }

    /* Sets a list to the lists map. */
    def setList(k: Symbol, v: List[Any]): Unit = {
      val map = getMap(k).y
      map(k) = v;
    }
    
    /* Sets any value to the bindings map. */
    def set(k: Symbol, v: Any): Unit = {
      val map = getMap(k).x
      map(k) = v;
    }

    /* Returns an integer. */
    def num(k: Symbol): Int = {
      any(k) match {
        case n: Int => n
        case _ => throw new RuntimeException(f"Variable $k does not exist or is not an integer")
      }
    }

    /* Returns ints and doubles. */
    def anyval(k: Symbol): AnyVal = {
      any(k) match {
        case n: Int => n
        case n: Double => n
        case _ => throw new RuntimeException(f"Variable $k does not exist as type AnyVal")
      }
    }
    

    /* Returns anything. */
    def any(k: Symbol): Any = {
      val map = getMap(k).x
      map.get(k) match {
        case Some(x) => x
        case None => None
      }
    }
     
    /* Returns a list of type any. */
    def anyList(k: Symbol): List[Any] = {
      val map = getMap(k).y
      map.get(k) match {
        case Some(x) => x
        case None => List()
      }
    }
    
    /* Returns an array of type any. */
    def arrayval(k: Symbol): Array[Any] = {   
      any(k) match {    
        case n: Array[Any] => n   
        case _ => throw new RuntimeException(f"Variable $k does not exist as type Array")   
      }   
    }

    override def toString: String = {
      bindingsStack.top.toString
    }
  }
}
