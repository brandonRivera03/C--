

package cmm

object cmmTicTacToe extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Variable('gameBoard) is List(0,1,2,3,4,5,6,7,8)
    Variable('playLocation) is 0
    Variable('userTurn) is between(0,1)
    Variable('gameWon) is false
    Variable('gameWinner) is "Nobody"

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

    If(('idx1 isEquivalentTo 'idx2) and ('idx2 isEquivalentTo 'idx3))
      Variable('gameWon) is true
    EndIf
    If(('idx4 isEquivalentTo 'idx5) and ('idx5 isEquivalentTo 'idx6))
      Variable('gameWon) is true
    EndIf
    If(('idx7 isEquivalentTo 'idx8) and ('idx8 isEquivalentTo 'idx9))
      Variable('gameWon) is true
    EndIf

    If(('idx1 isEquivalentTo 'idx4) and ('idx4 isEquivalentTo 'idx7))
      Variable('gameWon) is true
    EndIf
    If(('idx2 isEquivalentTo 'idx5) and ('idx5 isEquivalentTo 'idx8))
      Variable('gameWon) is true
    EndIf
    If(('idx3 isEquivalentTo 'idx6) and ('idx6 isEquivalentTo 'idx9))
      Variable('gameWon) is true
    EndIf

    If(('idx1 isEquivalentTo 'idx5) and ('idx5 isEquivalentTo 'idx9))
      Variable('gameWon) is true
    EndIf
    If(('idx3 isEquivalentTo 'idx5) and ('idx5 isEquivalentTo 'idx7))
      Variable('gameWon) is true
    EndIf
    If(('idx1 isNotEquivalentTo 0) and ('idx2 isNotEquivalentTo 1) and ('idx3 isNotEquivalentTo 2) and ('idx4 isNotEquivalentTo 3) and ('idx5 isNotEquivalentTo 4) and ('idx6 isNotEquivalentTo 5) and ('idx7 isNotEquivalentTo 6) and ('idx8 isNotEquivalentTo 7) and ('idx9 isNotEquivalentTo 8))
      Variable('endGame) is true
    EndIf

    EndFunction
    
    Function('updateBoard)
    
    EndFunction

    Print("Let's Play Tic-Tac-Toe!!!")
    Print()
    While(true)
      Call('printBoard)
      If(('endGame isEquivalentTo true) and ('gameWon isEquivalentTo false))
          Print('gameWinner, " is the victor. It's a draw!")
          Break
      EndIf
      If(('gameWon isEquivalentTo true) and ('gameWinner isEquivalentTo "player"))
        Print("Tic-Tac-Toe!!!!")
        Print("Congratulations! You have beater the computer and won the game!")
        Break
      EndIf
      If(('gameWon isEquivalentTo true) and ('gameWinner isEquivalentTo "computer"))
        Print("Tic-Tac-Toe!!!!")
        Print("Sorry. The computer has bested you and has won the game. :(")
        Break
      EndIf
      If('userTurn isEquivalentTo 1)
        Print("User's turn")
        Print("Your turn, please type the location you would like to play on. (X)")
        Input('userSelection)
        Variable('playLocation) is get('gameBoard, 'userSelection)
        While(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          Print("You cannot choose that spot, please select another location (X)")
          Input('userSelection)
          Variable('playLocation) is get('gameBoard, 'userSelection)
        Done
        Variable('gameBoard) is update('gameBoard, 'userSelection, "X")
        Call('checkWin)
        If('gameWon isEquivalentTo true)
          Variable('gameWinner) is "player"
        EndIf
        Print()
        Print("=========================")
        Print()
      EndIf
      If('userTurn isEquivalentTo 0)
        Print("Computer's Turn")
        Variable('computerSelection) is between(0,8)
        Variable('playLocation) is get('gameBoard, 'computerSelection)
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("cookie")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("msokpjm")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("pwekrm")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("kjlsdf")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("sjlkp")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("asjk")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("uowejs")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("fh238h")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("fh8u23hu")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("b7d89f798")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("ba80923ysd")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("nbpnq3nd")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        If(('playLocation isEquivalentTo "X") or ('playLocation isEquivalentTo "O"))
          EmptyVariable("9uvb0nens")
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        EndIf
        Print("The computer has placed his marker (O) on ", 'playLocation)
        Variable('gameBoard) is update('gameBoard, 'computerSelection, "O")
        Call('checkWin)
        If('gameWon isEquivalentTo true)
          Variable('gameWinner) is "computer"
        EndIf
        Print()
        Print("=========================")
        Print()
      EndIf
      If('userTurn isEquivalentTo 1)
        Variable('userTurn) is 0
      Else
        Variable('userTurn) is 1
      EndIf
    Done
    End
  }
}
