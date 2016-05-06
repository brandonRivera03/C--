package cmm

object test_Print_String extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Print("hello world")
    End
  }
}

object test_Print_Values extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Print(100)
    Print(333.33)
    End
  }
}

object test_Error extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Print("Running Error test...")
    Error("Encountered string error!")
    Error(404)
    Error(40.4)
    End
  }
}

object test_Simple_Assign_Ints extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Variable('x) is 1
    Print('x)
    Variable('y) equals 10
    Print('y)
    Variable('z) isDefinedAs 100
    Print('z)
    End
  }
}

object test_Simple_Assign_Doubles extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Variable('x) is 1.02
    Print('x)
    Variable('y) equals 10.03
    Print('y)
    Variable('z) isDefinedAs 100.105
    Print('z)
    End
  }
}

object test_Simple_Assign_String extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Variable('x) is "x is"
    Print('x)
    Variable('y) equals "y equals"
    Print('y)
    Variable('z) isDefinedAs "z isDefinedAs"
    Print('z)
    End
  }
}

object test_Simple_Assign_List extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Variable('x) is List(10,20,30,40)
    Print('x)
    Variable('y) equals List("ab", "cd", "efg", "h")
    Print('y)
    Variable('z) isDefinedAs List("var", 19, "equals", 19.0)
    Print('z)
    End
  }
}
