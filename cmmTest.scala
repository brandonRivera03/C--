

package cmm

object cmmTest extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Print("Welcome to the guessing game!")
    Print("Each turn you can guess a number between 1 and 20.")
    Print("We will give you hints regarding your guess.")
    Print("--------------")
    Variable('number) is between(1, 20)
    Variable('attempts) is 1
    While(true)
      //Variable('test) is 10
      Print("Guess a number between 1 and 20 (inclusive)")
      Input('guess)
      If('guess isLessThan 'number)
      	Print ("Higher!")
      EndIf
      If('guess isGreaterThan 'number)
      	Print("Lower!")
      EndIf
      If('guess isEqualTo 'number)
      	Print("You got it!")
      	Break
      EndIf
      Variable ('attempts) is ('attempts plus 1)
    Done
    Print("It took you", 'attempts, "turns!")
    End
  }
}