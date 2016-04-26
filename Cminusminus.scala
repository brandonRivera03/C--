package cmm

import scala.collection.mutable.{ HashMap, Stack }
import scala.util.Random
import scala.math.{ min, max }

class Cminusminus {
  abstract sealed class C_Line
  case class PrintString(num: Int, s: String) extends C_Line
  case class PrintVariable(num: Int, s: Symbol) extends C_Line
  case class PrintNumber(num: Int, s: Int) extends C_Line
  case class PrintFunction(num: Int, s: Function0[Any]) extends C_Line
  case class PrintMany(num: Int, s: Vector[Any]) extends C_Line
  case class ReadString(num: Int, s: Symbol) extends C_Line
  case class ErrorPrintString(num: Int, s: String) extends C_Line
  case class ErrorPrintVariable(num: Int, s: Symbol) extends C_Line
  case class ErrorPrintNumber(num: Int, s: Int) extends C_Line
  case class ErrorPrintFunction(num: Int, s: Function0[Any]) extends C_Line
  case class ErrorPrintMany(num: Int, s: Vector[Any]) extends C_Line
  case class IfStatement(num: Int, fun: Function0[Boolean]) extends C_Line
  case class IfSymb(num: Int, sym: Symbol) extends C_Line
  case class StartFalse(num: Int) extends C_Line
  case class EndIfStatement(num: Int) extends C_Line
  case class Assign(num: Int, fn: Function0[Unit]) extends C_Line
  case class LoopBeg(bool: Boolean) extends C_Line
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

  def ifFalse() = {
    lines(current) = StartFalse(current)
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
    def is(v: Function0[Any]): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v())))
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
    def equals(v: Function0[Any]): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v())))
      current += 1
    }
  }

  /* Runtime Evaluation. */
  
  private def gotoLine(line: Int) {

    def GeneralIf(bool: Boolean): Unit = {
      if (bool) {  /* If the conditional was true. */
        gotoLine(line + 1)
      } 
      else {       /* Find next statement we need to run. */
        var curLine = line
        var advance = true;
        while (advance) {
          curLine += 1
          if (lines(curLine).isInstanceOf[IfStatement] 
              || lines(curLine).isInstanceOf[EndIfStatement] 
              || lines(curLine).isInstanceOf[StartFalse]) 
          {
            advance = false
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
        println(binds.any(s))
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
        binds.set(s, value)
        gotoLine(line + 1)
      }

      case IfStatement(_, fun: Function0[Boolean]) => {
        GeneralIf(fun())
      }

      case IfSymb(_, sym: Symbol) => {
        GeneralIf(binds.any(sym).asInstanceOf[Boolean])
      }

      case StartFalse(_) => {
        var curLine = line
        while (!lines(curLine).isInstanceOf[EndIfStatement]) {
          curLine = curLine + 1;
        }
        gotoLine(curLine + 1);
      }

      case EndIfStatement(_) => {
        gotoLine(line + 1);
      }

      case Assign(_, fn: Function0[Unit]) =>
        {
          fn()
          gotoLine(line + 1)
        }
        
      case LoopBeg(bool: Boolean) => {
        if (bool) {
          gotoLine(line + 1)
        }
        else {
          BreakLoop()
        }
      }
      
      case BreakLoop() => {
        var curLine = line
        var advance = true;
        while (advance) {
          curLine += 1
          if (lines(curLine).isInstanceOf[LoopEnd]) {
            advance = false;
          }
        }
        gotoLine(curLine + 1)
      }
      case LoopEnd(loopBegLine: Int) => {
        gotoLine(loopBegLine + 1)
      }
      case FuncBeg(name: Symbol) => {
        var lineVar = line
        while (!lines(lineVar).isInstanceOf[FuncEnd]) {
          lineVar += 1
        }
        gotoLine(lineVar + 1)
      }
      case FuncEnd() => {

        // always pop from returnStack
        val temp: Any = returnStack.pop()

        binds.leaveScope()

        // TODO add more options
        temp match {
          case t: Function0[Any] => {
            if (returnStack.length > 0) {
              returnStack.pop() match {
                case v: Symbol => {
                  // set our return variable
                  binds.set(v, t())
                }
                case v => {
                  v match {
                    case None => // throw both away
                    case _ => {
                      // oops both were good values
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
                  // set our return variable
                  binds.set(v, t)
                }
                case v => {
                  v match {
                    case None => // throw both away
                    case _ => {
                      // oops both were good values
                      returnStack.push(v)
                      returnStack.push(t)
                    }
                  }
                }
              }
            }
          }
          case t: Symbol => {
            // oops we popped a symbol
            returnStack.push(t)
          }
          case None =>
          case _ => throw new RuntimeException(f"Something bad has happened! $temp")
        }

        gotoLine(pcStack.pop())
      }
      case FuncReturn(value: Any) => {

        // check and evaluate the types
        value match {
          case v: Function0[Any] => returnStack.push(v())
          case v: Symbol => returnStack.push(binds.any(v))
          case v => returnStack.push(v)
        }

        // actually need to go to end of function
        var lineVar = line
        while (!lines(lineVar).isInstanceOf[FuncEnd]) {
          lineVar += 1
        }
        gotoLine(lineVar)
      }
      case FuncCall(funcName: Symbol) => {
        // push trash onto the return stack
        returnStack.push(None)
        pcStack.push(line + 1)
        binds.newScope()
        gotoLine(funcBegLines.get(funcName) match {
          case Some(s) => s + 1 //go beyond the start of the function
          case None => -1
        })
      }
      case FuncCallReturn(funcName: Symbol, variable: Symbol) => {
        // push the return variable onto the return stack
        returnStack.push(variable)
        pcStack.push(line + 1)
        binds.newScope()
        gotoLine(funcBegLines.get(funcName) match {
          case Some(s) => s + 1 // go beyond the start of the function
          case None => -1
        })
      }
      case EndStatement(_) =>
      case _ =>
    }
  }

  /* Random number functions. */
  def between(i: Int, j: Int): Int = { random.nextInt(j + 1 - i) + i }

  /* Maximum and Minimum functions. */
  def maxOf(i: Any, j: Any): Function0[Any] = {
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

  def minOf(i: Any, j: Any): Function0[Any] = {
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

  /* Basic Mathematical Operations. */
  implicit def operator_any(i: Any) = new {
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

  }

  object Input {
    def apply(s: Symbol) = {
      lines(current) = ReadString(current, s)
      current += 1
    }
  }

  /**
   * attempt to convert String to an Integer
   * if not possible, return the original String
   */
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

  object variable_default {
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
  }

  def While(bool: Boolean) {
    lines(current) = LoopBeg(bool)
    loopBegLines.push(current)
    current += 1
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

  object Send {
    def apply(value: Any) = {
      lines(current) = FuncReturn(value)
      current += 1
    }
  }

  def FunctionEnd {
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

  class Bindings {
    val bindingsStack = Stack[HashMap[Symbol, Any]]()
    val bindings = HashMap[Symbol, Any]()

    /*
     * Create a new scope.
     * Call whenever doing a function call.
     */
    def newScope() {
      bindingsStack.push(new HashMap[Symbol, Any])
    }

    /*
     * Destroy topmost scope.
     * Call whenever leaving a function.
     */
    def leaveScope() {
      bindingsStack.pop()
    }

    /**
     * get correct HashMap for your scope
     */
    def getMap(sym: Symbol): HashMap[Symbol, Any] = {
      val bindingsStackCopy = Stack[HashMap[Symbol, Any]]()
      val bindingsStackTop = bindingsStack.top
      while (!bindingsStack.isEmpty && !bindingsStack.top.contains(sym)) {
        bindingsStackCopy.push(bindingsStack.pop())
      }
      //bindingsStackCopy.push(bindingsStack.pop())
      var map = bindingsStackTop
      if (!bindingsStack.isEmpty) {
        map = bindingsStack.top
      }
      while (!bindingsStackCopy.isEmpty) {
        bindingsStack.push(bindingsStackCopy.pop())
      }
      map
    }

    /**
     * set a value in our map
     */
    def set(k: Symbol, v: Any): Unit = {
      val map = getMap(k)
      map(k) = v;
    }

    /**
     * only returns integers
     */
    def num(k: Symbol): Int = {
      any(k) match {
        case n: Int => n
        case _ => throw new RuntimeException(f"Variable $k does not exist or is not an integer")
      }
    }

    /**
     * WARNING: don't use yet
     * returns ints and doubles
     */
    /* DID NOT TOUCH THIS WHILE IMPLEMENTING SCOPE */
    def anyval(k: Symbol): AnyVal = {
      any(k) match {
        case n: Int => n
        case n: Double => n
        case _ => throw new RuntimeException(f"Variable $k does not exist as type AnyVal")
      }
    }

    /**
     * returns anything
     */
    def any(k: Symbol): Any = {
      val map = getMap(k)
      map.get(k) match {
        case Some(x) => x
        case None => None
      }
    }

    override def toString: String = {
      bindingsStack.top.toString
    }
  }
}