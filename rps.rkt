;; Author: Arnold Gihozo
;; Class: AUCSC 370
;; Date Created: October 27th 2020
;; Date Last modified: October 30th 2020
;;
;; Description:
;; The program bellow executes a simple Rock Paper Scissor game between the computer and the user. This program
;; was created within the scope of the AUCSC 370 class at the University of Alberta- Augustana Campus. Its main goal
;; is to get use with the Scheme programming language. The RPS game last for 10 rounds and is played between a computer
;; and a user. The game is played over terminal where the user has to input either "rock, paper or scissor" any any
;; format and length. The program will accept the input as long as the first letter of the
;; input matches one of the 3 options, othewise, the user will be asked to try again. At the end of the 10 rounds,
;; the game will display the final score, the number of tied rounds, the final champion 
;;
;; NOTE: This program was created using Dr.Racket (since Chez Squeme was not available on this device)
;; therefore, parts that were specific to Dr.Racket (such as the import section and "random") have
;; been used but commented out for the purpose of the assingement. The assignement has only been tested
;; on Dr.Racket.
;;
;; 
;;=========================================================================================================
;; Import Scheme in Dr.Racket 
 #!r6rs
 (import (rnrs)
        (srfi :27))
;;=========================================================================================================


;;=========================================================================================================
;; Program Execution starts here
;; SECTION A: Getting User Input and Checking User Input
;;=========================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function is called in order to start the RockPaper Scission Game.
;; It calls the function gameFinish (with initializing the scores to zero)
;; (0 0 0):
;;     [0] = userScore
;;     [1] = computerScore
;;     [2]=  ties
(define (play)
  (display "Welcome to \"ROCK\" \"PAPER\" \"SCISSOR\" game \n")
  (display "----------------------------------- \n")
  (gameFinishLoop '(0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function is the main loop for our game. First, it checks
;; if the given list of scores equals to the number of rounds (10)
;; otherwise, it displays the prompt and gets the reads the user
;; input via the function readLine. Once the input is gotten, it then
;; proceeds to check if input is valid. 
;; 
(define (gameFinishLoop listScore)
  (cond
    ((= 10 (+ (car listScore) (cadr listScore) (caddr listScore))) (endGame listScore))
    (else (display "Please enter your choice: \"Rock\", \"Paper\", or \"Scissors\" ")(checkInput (userInput (readLine)) listScore))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLine() --> line (as String) ;;
;; Read one line from standard input, not including the newline
;; but eliminating it. This is wrapper for the recursive method
;; that does the work (readLoop).
;;
;; By Rosanna Heise
;;
(define (readLine)
(readLoop (read-char (current-input-port)) '())) ;do wait for one char

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLoop(currentCharacter line) --> line (as String) ;;
;;
;; This recursive method reads a character at a time from the
;; current input port (assuming Scheme's "Interaction Window")
;; until it finds the newline (i.e. enter). It builds the characters
;; into a string which is returned at the end. Newline is not part
;; of the string, but is eliminated from the input ;;
;;
;; By Rosanna Heise
;;
(define (readLoop curChar line)
(cond
((char=? #\newline curChar) (list->string line)) (else (readLoop (read-char (current-input-port))
(append line (list curChar))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; userInput(listOfInput) -> car(listOfInput) OR #\n
;;
;; This function will be called from gameFinish and get the first
;; element of the string inputed by the user. It will either
;; return the first letter within the user's input OR #\n
;; when the user hits"enter" instead of a character or string (which then the user
;; will be prompted to enter character again. The output from this function will check
;; if the given input is valid.
;; 
(define (userInput listOfInput)
  (cond
    ((null? (string->list listOfInput))#\n) ; if user enter "enter" instead of a character. Invalid "Dummy" character is given
    (else (car (string->list listOfInput)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; checkInput(aChar scoreList) --> string
;;
;; This function will take in the first character of a userInput. Checks
;; whether the character is valid (r,R,P,p,S,s) then converts it into a
;; string. Otherwise, it will reloop to the begining of the game and
;; asks the user to enter the correct input.
;;
(define (checkInput aChar scoreList) 
 (cond
   ((member aChar '(#\r #\R #\p #\P #\s #\S))(convertInputCharToString aChar scoreList))
   (else (display "Not Sure of your selection. Try again. \n")(gameFinishLoop scoreList))))

;;=========================================================================================================
;; Arriving here, we know that our userInput is valid, so this section will check the winner
;; SECTION B: Check for Winner and get random computer input
;;=========================================================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convertInputCharToString(aChar scoreList) --> userInputString AND computerInputString
;;
;; This function will take in a the first letter of a userInput word and converts it to
;; a string user the (convertInputCharToStringWorker) function. This function will
;; also geneerate a random computerInput and converts it to a string as well using the same
;; function.
;;
(define (convertInputCharToString aChar scoreList)
  (validInput(convertInputCharToStringHelper aChar)(convertInputIntToString (computerInput)) scoreList))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convertInputCharToString(aChar) --> string
;;
;; This is one of the helper function that will take a given(valid) character
;; and convert it to its given string.
;;
(define (convertInputCharToStringHelper aChar)
  (cond
    ((member aChar '(#\r #\R)) "ROCK") 
     ((member aChar '(#\p #\P)) "PAPER")
      ((member aChar '(#\s #\S)) "SCISSOR")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; computerInput() --> random integer
;;
;; This function does not have any parameters but will generate
;; a random integer between 0-3 (not including 3).
;;
;; Numbers generated will be:
;;        0 --> ROCK, 1--> PAPER, 2 -->SCISSOR
;;
;; NOTE: In Dr.Racket, this function is implemeted using random-integer
;; instead of random.
;;
(define (computerInput)
  (random-integer 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convertInputIntToString(computerChoiceInt) --> string
;;
;; This function will take in a random computer generated integer
;; and will convert the randomly generated numbers to strings.
;; As reminder 0 means ROCK, 1 means PAPER, 2 means SCISSOR.
;;
(define (convertInputIntToString computerChoiceInt)
 (cond
    ((equal? computerChoiceInt 0) "ROCK") 
     ((equal? computerChoiceInt 1) "PAPER")
      ((equal? computerChoiceInt 2) "SCISSOR")))

;;=========================================================================================================
;; Arriving here, we have 2 strings corresponding to our computer and user choices. 
;; SECTION C: Print Choices, update points, Check winner and check whether game is finish
;;=========================================================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; validInput (userChoiceString computerChoiceString scoreList) --> listofScores
;;
;; This function will take in a valid user inputs to display as well as the scoreList
;; It will display those inputs (execpt for score) and will call the following functions:
;;      1. checkWinner --> to find out who won between computer and user
;;      2. roundTracler --> to update the scoreList based on who won.
;; These two functions will also have helper conversion functions to achieve the goal
;; of the outer function.
;; The function will display the user and computer input, and the winner of the round
;;
(define (validInput userChoiceString computerChoiseString scoreList)
  (display "You entered ")
  (display userChoiceString)
  (display " Computer chose ")
  (display computerChoiseString)
  (display "  ")
  (display (checkWinner (convertStringToInt userChoiceString)(convertStringToInt computerChoiseString)))
  (display "\n \n")
  (gameFinishLoop (roundTracker scoreList (checkWinner (convertStringToInt userChoiceString)(convertStringToInt computerChoiseString)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; checkWinner(inputUserInt inputComputerInt) --> string of winner of round
;;
;; This function will take in the valid user and computer input and checks for the winner of the round. It will first
;; check if the two inputs are equal. Then it will use modulo to evaluate whether the user or computer has won. The
;; way it is doing this is by "thinking of an imaginary circle". In other words, we know that ROCK > SCISSORS >
;; PAPER > ROCK > SCISSORS etc... (its a circle going on).
;;
;; This function then will always check for the next option after user input. If the next input (ie:computer input),
;; is the same as the user input, then that means the user has lost. If it is not tho, that means the computer
;; input is before user input, meaning the user has won. Hence why this function only has 3 checks (tie, userWin or
;; computerWin)
;;
(define (checkWinner inputUserInt inputComputerInt) 
  (cond
    ((= inputUserInt inputComputerInt)"TIE")
    ((=(mod (+ 1 inputUserInt) 3)(mod inputComputerInt 3))"Winner is COMPUTER")
    (else "Winner is YOU")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convertStringToInt (aString) --> int
;;
;; In order to execute checkWinner, we need to build this helper function
;; that will convert a given string to its corresponding integer.
;; 0 --> ROCK, 1--> PAPER, 2 -->SCISSOR
;;
(define (convertStringToInt aString)
  (cond
    ((equal? aString "ROCK")0)
    ((equal? aString "PAPER")1)
    ((equal? aString "SCISSOR")2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; roundTracker (scoreList winnerString) --> anUpdated list of scores
;;
;; This function will take in our list of scores and the string
;; of the winning output (either tie, winner is computer or winner is you)
;; and use it to determine to whom it may assign the points to. It
;; will then return an updated list of the scores.
;;
(define (roundTracker scoreList winnerString)
  (cond
    ((equal? winnerString "TIE") (map + '(0 0 1) scoreList))
    ((equal? winnerString "Winner is COMPUTER") (map + '(0 1 0) scoreList))
    ((equal? winnerString "Winner is YOU") (map + '(1 0 0) scoreList))))

;;=========================================================================================================
;; Arriving here, it means that our gameFinish has reached the end of our rounds
;; SECTION D: Print the final output of the program (who won, user points, computer points and ties)
;;=========================================================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; endGame (listScore) --> nothing
;;
;; This function will take in the final scores and display them.
;; It will also determine the winner of the game by comparing largest
;; position within the list.
;; (0 0 0):
;;     [0] = userScore
;;     [1] = computerScore
;;     [2]=  ties
;;
(define (endGame listScore)
  (display "Game over! You won ")
  (display (car listScore))
  (display " ,computer won ")
  (display (cadr listScore))
  (display " ,tied ")
  (display (caddr listScore))
  (display " rounds")
  (newline)
  (cond
    ((> (car listScore) (cadr listScore)) (display "Champion: YOU"))
    ((< (car listScore) (cadr listScore)) (display "Champion: COMPUTER"))
    (else (display "No one wins! It is a tie!"))))



;; END
;;=========================================================================================================




  