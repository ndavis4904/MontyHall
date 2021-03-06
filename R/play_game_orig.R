#' Original Monty Hall game with 2 goats and 1 car
#' 
#' @param ... further arguments passed to or from other methods.
#' 
#' @return The number of goats added to the number of cars makes the total number of doors to choose from.
#' @examples
#' ## Single simulation of the original Monty Hall Game
#' play_game()
#' 
#' @export
play_game_orig <- function( )
{
  create_game <- function( )
  {
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
  } 
  
  select_door <- function( )
  {
    doors <- c(1,2,3) 
    a.pick <- sample( doors, size=1 )
    return( a.pick )  # number between 1 and 3
  }
  
  open_goat_door <- function( game, a.pick )
  {
    doors <- c(1,2,3)
    # if contestant selected car,
    # randomly select one of two goats 
    if( game[ a.pick ] == "car" )
    { 
      goat.doors <- doors[ game != "car" ] 
      opened.door <- sample( goat.doors, size=1 )
    }
    if( game[ a.pick ] == "goat" )
    { 
      opened.door <- doors[ game != "car" & doors != a.pick ] 
    }
    return( opened.door ) # number between 1 and 3
  }
  
  change_door <- function( stay=T, opened.door, a.pick )
  {
    doors <- c(1,2,3) 
    
    if( stay )
    {
      final.pick <- a.pick
    }
    if( ! stay )
    {
      final.pick <- doors[ doors != opened.door & doors != a.pick ] 
    }
    
    return( final.pick )  # number between 1 and 3
  }
  
  determine_winner <- function( final.pick, game )
  {
    if( game[ final.pick ] == "car" )
    {
      return( "WIN" )
    }
    if( game[ final.pick ] == "goat" )
    {
      return( "LOSE" )
    }
  }
  
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("Stay","Switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}