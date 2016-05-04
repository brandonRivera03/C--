

package cmm

object cmmTicTacToe extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Variable('gameBoard) is List(1,2,3,4,5,6,7,8,9)
    Variable('userTurn) is between(0,1)
    Variable('gameWon) is false
    Variable('gameWinner) is "Nobody"
    Variable('endGame) is false

    Function('printBoard)
    Variable('idx1) is get('gameBoard, 0)
    Variable('idx2) is get('gameBoard, 1)
    Variable('idx3) is get('gameBoard, 2)
    Variable('idx4) is get('gameBoard, 3)
    Variable('idx5) is get('gameBoard, 4)
    Variable('idx6) is get('gameBoard, 5)
    Variable('idx7) is get('gameBoard, 6)
    Variable('idx8) is get('gameBoard, 7)
    Variable('idx9) is get('gameBoard, 8)
    Variable('horizontalLine) is "-----+-----+-----"

    Print(" ", 'idx1, " | ", 'idx2, " | ", 'idx3)
    Print('horizontalLine)
    Print(" ", 'idx4, " | ", 'idx5, " | ", 'idx6)
    Print('horizontalLine)
    Print(" ", 'idx7, " | ", 'idx8, " | ", 'idx9)
    Print()
    Print()
    EndFunction

    Function('checkWin)
    Variable('idx1) is get('gameBoard, 0)
    Variable('idx2) is get('gameBoard, 1)
    Variable('idx3) is get('gameBoard, 2)
    Variable('idx4) is get('gameBoard, 3)
    Variable('idx5) is get('gameBoard, 4)
    Variable('idx6) is get('gameBoard, 5)
    Variable('idx7) is get('gameBoard, 6)
    Variable('idx8) is get('gameBoard, 7)
    Variable('idx9) is get('gameBoard, 8)

    If(true)
      If('idx1 == 'idx2 and 'idx2 == 'idx3)
        Variable('gameWon) is true
      EndIf
      If('idx4 == 'idx5 and 'idx5 == 'idx6)
        Variable('gameWon) is true
      EndIf
      If('idx7 == 'idx8 and 'idx8 == 'idx9)
        Variable('gameWon) is true
      EndIf

      If('idx1 == 'idx4 and 'idx4 == 'idx7)
        Variable('gameWon) is true
      EndIf
      If('idx2 == 'idx5 and 'idx5 == 'idx8)
        Variable('gameWon) is true
      EndIf
      If('idx3 == 'idx6 and 'idx6 == 'idx9)
        Variable('gameWon) is true
      EndIf

      If('idx1 == 'idx5 and 'idx5 == 'idx9)
        Variable('gameWon) is true
      EndIf
      If('idx3 == 'idx5 and 'idx5 == 'idx7)
        Variable('gameWon) is true
      EndIf
    Else
      If('idx1 isNot 1 and 'idx2 isNot 2 and 'idx3 isNot 3)
        If('idx4 isNot 4 and 'idx5 isNot 5 and 'idx6 isNot 6)
          If('idx7 isNot 7 and 'idx8 isNot 8 and 'idx9 isNot 9)
            Variable('endGame) is true
          EndIf
        EndIf
      EndIf
    EndIf

    EndFunction

    Print("Let's Play Tic-Tac-Toe!!!")
    Print()
    Variable('count) is 0
    While(true)
      Variable('count) is 'count plus 1
      Print('count)
      If(('endGame == true) or ('gameWon == true))
        Call('printBoard)
        If('gameWon isNot true)
          Print('gameWinner, " is the victor. It's a draw!")
          Break
          // End
        Else
          If('gameWinner == "player")
            Print("Tic-Tac-Toe!!!!")
            Print("Congratulations! You have beater the computer and won the game!")
          EndIf
          If('gameWinner == "computer")
            Print("Tic-Tac-Toe!!!!")
            Print("Sorry. The computer has bested you and has won the game. :(")
          EndIf
          Break
          // End
        EndIf
      EndIf
      If('userTurn == 1)
        Print("User's turn")
        Call('printBoard)
        Print("Your turn, please type the location you would like to play on. (X)")
        Input('userSelection)
        Variable('playLocation) is get('gameBoard, 'userSelection)
        While('playLocation == 'X' or 'playLocation == 'O')
          Print("You cannot choose that spot, please select another location (X)")
          Input('userSelection)
          Variable('playLocation) is get('gameBoard, 'userSelection)
        Done
        Variable('gameBoard) is update('gameBoard, 'playLocation, "X")
        Call('checkWin)
        If('gameWon == true)
          Variable('gameWinner) is "player"
        EndIf
        Print()
        Print("=========================")
        Print()
        Variable('userTurn) is 0
      Else
        Print("Computer Turn")
        Call('printBoard)
        Variable('computerSelection) is between(1,9)
        Variable('playLocation) is get('gameBoard, 'computerSelection)
        While('playLocation == 'X' or 'playLocation == 'O')
          Variable('computerSelection) is between(1,9)
          Variable('playLocation) is get('gameBoard, 'userSelection)
        Done
        Print("The computer has placed his marker (O) on ", 'playLocation)
        Variable('gameBoard) is update('gameBoard, 'playLocation, "O")
        Call('checkWin)
        If('gameWon == true)
          Variable('gameWinner) is "computer"
        EndIf
        Print()
        Print("=========================")
        Print()
        Print('userTurn)
        Variable('userTurn) is 1
        Print("End of Computer's Turn")
      EndIf
    Done
    End
  }
}