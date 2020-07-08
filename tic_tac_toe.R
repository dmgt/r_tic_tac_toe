# Tic-tac-toe in R 
# By Dana Miller

# This file contains 5 functions

##### Helper functions
  # initial_prompt()
  # player2_initial_prompt()
  # midgame_prompt(player_number)

##### Function to plot the 3 x 3 game board
  # plot_board(player_1_moves, player_2_moves)

##### Main game function
  # tic_tac_toe

################################# Load packages ####################################
# These are all common R packages (standard packages used with the 'tidyverse' of data analysis tools)
library(glue)  # for helpful defaults for print statements in R
library(purrr) # For functions to map over a vector (specifically `map_int()` below)
library(magrittr) # For 'pipe' operator ( %>% ) to specify operations from left to right w/o nested function calls


########################## Helper functions for user input ##########################


initial_prompt <- function()  {
  # Prompt for first play of game only, has extra information about how to play
  #    Input : none (but asks for user input at prompt)
  #    Output: List, with 1 numeric value 

  #Initialize
  player_1_moves <- c()
  player_2_moves <- c()
  
  print(glue("
                                                                     
             Welcome to tic-tac-toe!
             
             This is a game for two players. The goal is to get three x's or o's in a row on the board 
             (in any order - horizontal, vertical, or diagonal)

             You can take turns entering your move at this prompt. 
             There are a total of 9 positions on the board, each indicated with a number, as shown below:
             
             "))

  plot_board(player_1_moves, player_2_moves)
  
  print(glue("
             
                          Player 1 will go first. Your plays will use the 'x' symbol on the board. Player 2 is 'o'.
             
             "))
  
  
  n <- (readline("Player 1, what's your first move? Please enter a number from 1-9: "))
  n <- as.numeric(n)
  list(n=n)
}


player2_initial_prompt <- function(){
  # Prompt for player 2's first play only (extra wording for first play)
  #    Input : none (but asks for user input at prompt)
  #    Output: List, with 1 numeric value 
  
  n <- (readline("Player 2, it's time for your first move with 'o', please enter a number from 1-9: "))
  n <- as.numeric(n)
  list(n=n)
}

midgame_prompt <- function(player_number){
  # Prompt used for both players 1 and 2, repeated, during middle rounds of play
  #    Input : Integer (1 or 2)
  #    Output: List, with 1 numeric value 
  
  n <- (readline(glue("Player {player_number}, your turn. Please enter a number from 1-9: ")))
  n <- as.numeric(n)
  list(n=n)
}

play_again_prompt <- function() {
  # Prompt after game ends
  # Input: None (but asks for user input at prompt)
  # Output: List with one string
  
  c <- (readline(glue("Would you like to play again? Please enter Y for 'Yes' or N for 'No': ")))
  c <- as.character(c)
  
  list(c = c)
}  

####################### Function to plot the 3 x 3 game board ########################

plot_board <- function(player_1_moves, player_2_moves ) {
  
  # Function to plot a 3 x 3 tic tac toe board, with digits (default) and x's and o's for played moves
  # Inputs: Up to two vectors of numbers between 1 and 9 representing plays by each player
  # Outputs: Text output printed to console
  
  # Notes- shows all digits if player_moves is NULL 
  # Adapted (quite a bit) from an example w/ a different represention of plays https://www.robert-hickman.eu/post/r-inforcement_learning_one/
  
  board_numbers <- c(1:9)
  
  for (i in board_numbers)
  {
    if (i %in% player_1_moves)  board_numbers[i] <- "x"
    if (i %in% player_2_moves)  board_numbers[i] <- "o"
  }
  
  board_elements <- glue_collapse(board_numbers, sep = " | ")
  
  board_lines <- gsub("(. \\| . \\| . )\\|( . \\| . \\| . )\\|( . \\| . \\| .)", #regex
                      "\n \\1\n-----------\n\\2\n-----------\n\\3",
                      board_elements
  )
  return(writeLines(board_lines))
}

################################# Main game function #################################

tic_tac_toe <- function ()  {
  # Function to execute game of tic-tac-toe
  # Requires all helper functions above
  # Inputs: None (but prompts for user input at prompt, numerous times until game is over)
  
  # Initialize empty vectors to store moves from each player
  player_1_moves <- c()
  player_2_moves <- c()
  
  # Initialize state for input prompts
  need_reenter_p1 <- T
  need_reenter_p2 <- T
  
  #### Function to check if input is numeric and has not been called already ####
  ## Note: Is inside tic_tac_toe function because otherwise `player_1_moves` is
  ## not found when this function called from inside the larger function
  
  test_numeric_move <- function(next_move) {
    
    # First check if input is numeric 
    if (is.na(next_move) == F) {   # Input is corerced to numeric in midgame_prompt, so NA if not a number
      error1 <- F
    } else { 
      error1 <- T
      print(glue("Oops! Looks like you entered something that isn't a number, so your move isn't on the board yet.
                            Please try again and enter a number between 1 and 9"))
    }
    
    # Second, check if input is within range 1 - 9, for inputs that are numeric (not NA)
    if (next_move %in% c(1,2,3,4,5,6,7,8,9)) { 
      error2 <- F
    } else if (is.na(next_move)){ error2 <- T # no additional message, given message above
    } else {
      error2 <- T
      print(glue("Sorry, you entered '{next_move}', which is not a position on the board.
                            Please try again and enter a number between 1 and 9 that hasn't been played yet"))
    }
    
    # Third check if the numeric value has already been played elsewhere on the board (numbers can't overlap)
    if (next_move %in% player_1_moves | next_move %in% player_2_moves) {
      error3 <- T
      print(glue("Hmm, you entered '{next_move}', but someone has already used that spot on the board
                            Please try again and enter a position on the board that is still avalible"))
    } else { 
      error3 <- F 
    }
    
    # If either condition failed, need to prompt player to re-enter
    need_reenter <- ifelse(error1 == T | error2 == T | error3 == T, T, F) # If either error is T, then need_reenter is T 
  } # end of test_numeric_move      
  
  #### First move by player 1 ####
  
  while (need_reenter_p1 == T ){
    
    inputs <- initial_prompt() # Trigger text input prompt
    
    # Check content of input, and re-prompt player from start of loop if needed
    need_reenter_p1 <- test_numeric_move(p1_next_move <- inputs$n) # Extract value from text input prompt
    if ( need_reenter_p1 == T ) {
      next
    }else if (need_reenter_p1 == F) {
      player_1_moves <- append(player_1_moves, p1_next_move)
    }
  } # end of player 1's first while loop  
  
  
  plot_board(player_1_moves, player_2_moves) 
  
  
  #### First move by player 2 ####
  
  while (need_reenter_p2 == T ){
    
    inputs2 <- player2_initial_prompt() # Different text for player 2's first turn
    
    # Check content of input, and re-prompt player from start of loop if needed
    need_reenter_p2 <- test_numeric_move(p2_next_move <- inputs2$n) # Extract value from text input prompt
    if ( need_reenter_p2 == T ) {
      next
    }else if (need_reenter_p2 == F) {
      player_2_moves <- append(player_2_moves, p2_next_move)
    }
  } # end of player 2's first while loop  
  
  
  plot_board(player_1_moves, player_2_moves) 
  
  #### Initialize conditions for loop for subsequent turns ####
  results <- list(continue_play = T, winner = NA)
  
  #### Function to test for win conditions #### 
  evaluate_win_conditions <-function(player_1_moves, player_2_moves) {
    
    #Enumerate winning combinations of positions (as sets where elements can occur in any order)
    list_of_winning_moves <- list(c(1,2,3), c(4,5,6), c(7,8,9), c(1,4,7), c(2,5,8), c(3,6,9), c(1,5,9), c(3,5,7))
    
    # Map return a list, map_int simplifies the output to a vector of integers
    p1_results <- map_int(list_of_winning_moves, function(x) is.element(unlist(x), player_1_moves) %>% sum() )  
    p2_results <- map_int(list_of_winning_moves, function(x) is.element(unlist(x), player_2_moves) %>% sum() ) 
    
    if (max(p1_results) == 3 ) {
      results$continue_play <- F
      results$winner <- "Player 1" 
    } else if (max(p2_results) == 3 ){ 
      results$continue_play <- F
      results$winner <- "Player 2" 
    } else if ( length(player_1_moves) + length(player_2_moves) == 9 ){
      results$continue_play <- F
      results$winner <- "both of you, you tied, so to both Player 1 and Player 2" 
    }
    
    results <- list(continue_play = results$continue_play, winner =  results$winner)
  }
  
  #### While loop with next moves by both players, including input checking ####
  # Version w/ two while loops inside while loop for (not optimal) error handling
  
  while (results$continue_play == T ){
    
    # (Re)initialize for each round of play
    need_reenter_p1 <- T
    need_reenter_p2 <- T
    
    # Player 1
    while (need_reenter_p1 == T ){
      
      player1_inputs <- midgame_prompt(player_number = 1)
      
      # Check content of input, and re-prompt player from start of loop if needed
      need_reenter_p1 <- test_numeric_move(p1_next_move <- player1_inputs$n)
      if ( need_reenter_p1 == T ) {
        next
      }else if (need_reenter_p1 == F) {
        player_1_moves <- append(player_1_moves, p1_next_move)
      }
    } # end of player 1 while loop
    
    plot_board(player_1_moves, player_2_moves)
    
    results <- evaluate_win_conditions(player_1_moves, player_2_moves) # Check if either player has won yet
    
    if (results$continue_play == F) { break }
    else {
      
      # Player 2
      while (need_reenter_p2 == T ){
        
        player2_inputs <- midgame_prompt(player_number = 2)
        
        # Check content of input, and re-prompt player from start of loop if needed
        need_reenter_p2 <- test_numeric_move(p2_next_move <- player2_inputs$n)
        if ( need_reenter_p2 == T ) {
          next
        }else if (need_reenter_p2 == F) {
          player_2_moves <- append(player_2_moves, p2_next_move)
        }
      } # end of player 2 while loop
      
      plot_board(player_1_moves, player_2_moves)
      
      results <- evaluate_win_conditions(player_1_moves, player_2_moves) # Check if either player has won yet
      
    } # end of else statement
  } # end of umbrella while loop
  
  #### End of game  - check if want to play again ####
  
  print(glue("Congrats, {results$winner}, you won! "))
  
  yes_no <- play_again_prompt()
  yes_no_choice <- yes_no$c
  
  if(yes_no_choice %in% c("N","n")) {
    
    print(glue("Game over"))
    
  } else if(yes_no_choice %in% c("Y","y")) {
    
    tic_tac_toe()
    
  } else {print(glue("Sorry, '{yes_no_choice}' is not a valid input"))
    
    # Repeat choices above
    yes_no <- play_again_prompt()
    yes_no_choice <- yes_no$c
    
    if(yes_no_choice %in% c("N","n")) {
      print(glue("Game over"))
    } else if(yes_no_choice %in% c("Y","y")) {
      tic_tac_toe()
    } else {print(glue("Sorry, this doesn't seem to be working out. Game over."))}
    
  } # End of else statement
  
} # End of function


