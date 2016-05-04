

package cmm

object cmmTicTacToe extends Cminusminus {
  def main(args: Array[String]): Unit = {
    Begin
    Variable('gameBoard) is List(1,2,3,4,5,6,7,8,9)
    Variable('playLocation) is 0
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
    If(('idx1 isNot 1) and ('idx2 isNot 2) and ('idx3 isNot 3) and ('idx4 isNot 4) and ('idx5 isNot 5) and ('idx6 isNot 6) and ('idx7 isNot 7) and ('idx8 isNot 8) and ('idx9 isNot 9))
      Variable('endGame) is true
    EndIf

    EndFunction
    
    Function('updateBoard)
    
    EndFunction

    Print("Let's Play Tic-Tac-Toe!!!")
    Print()
    While(true)
      Call('printBoard)
      Print("%%%%%%%%%%%%%%%%%%%%%%%%%%")  // DEBUG
      Print('gameBoard)  // DEBUG
      Print("%%%%%%%%%%%%%%%%%%%%%%%%%%")  // DEBUG
      If(('endGame isEquivalentTo true) and ('gameWon isEquivalentTo false))
          Print('gameWinner, " is the victor. It's a draw!")
          Break
          // End
      EndIf
      If(('gameWon isEquivalentTo true) and ('gameWinner isEquivalentTo "player"))
        Print("Tic-Tac-Toe!!!!")
        Print("Congratulations! You have beater the computer and won the game!")
        Break
        //End
      EndIf
      If(('gameWon isEquivalentTo true) and ('gameWinner isEquivalentTo "computer"))
        Print("Tic-Tac-Toe!!!!")
        Print("Sorry. The computer has bested you and has won the game. :(")
        Break
        //End
      EndIf
      If('userTurn isEquivalentTo 1)
        Print('gameBoard)  // DEBUG
        Print("User's turn")
        Print("Your turn, please type the location you would like to play on. (X)")
        Input('userSelection)
        Variable('userSelection) is ('userSelection minus 1)
        Print("userSelection: ", 'userSelection) // DEBUG
        Variable('playLocation) is get('gameBoard, 'userSelection)
        While(('playLocation isEquivalentTo 'X') or ('playLocation isEquivalentTo 'O'))
          Print("You cannot choose that spot, please select another location (X)")
          Input('userSelection)
          Variable('userSelection) is ('userSelection minus 1)
          Variable('playLocation) is get('gameBoard, 'userSelection)
        Done
        Print("Before update: ", 'gameBoard) // DEBUG
        Variable('gameBoard) is update('gameBoard, 'playLocation, "X")
        Print("After update: ", 'gameBoard) // DEBUG
        Print('gameBoard) // DEBUG
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
        While(('playLocation isEquivalentTo 'X') or ('playLocation isEquivalentTo 'O'))
          Variable('computerSelection) is between(0,8)
          Variable('playLocation) is get('gameBoard, 'computerSelection)
        Done
        Print("The computer has placed his marker (O) on ", 'playLocation)
        Print("Before update: ", 'gameBoard)  // DEBUG
        Variable('gameBoard) is update('gameBoard, 'playLocation, "O")
        Print("After update: ", 'gameBoard) // DEBUG
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
