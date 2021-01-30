#' Monty Hall game where user defines number of goats and cars
#' 
#' @param goats Numer of goats that will be hidden behind doors.
#' @param cars Number of cars that will be hidden behind doors.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return The number of goats added to the number of cars makes the total number of doors to choose from.
#' @examples
#' ## Runs a single observation of the Monty Hall problem with 2 goats and 1 car.
#' play_game(goats = 2, cars = 1)
#' 
#' ## Runs a single observation of the Monty Hall problem with 10 goats and 5 cars.
#' play_game(10, 5)
#' 
#' @export
play_game <- function( goats, cars )
{
  create_game_extended.2 <- function( goats, cars )
  {
    a.game <- sample( x=c(rep("goat",goats), rep("car", cars)), size = (goats + cars), replace=F )
    return( a.game )
  } 
  
  select_door_extended.2 <- function( door.choices )
  {
    doors <- c(1 : door.choices) 
    a.pick <- sample( doors, size=1 )
    return( a.pick )
  }
  
  open_goat_door_extended.2 <- function( game, a.pick, door.choices )
  {
    doors <- c(1 : door.choices)
    if( game[ a.pick ] == "car" )
    { 
      goat.doors <- doors[ game != "car" ] 
      opened.door <- sample( goat.doors, size=1 )
    } else
      if( game[ a.pick ] == "goat" )
      { 
        opened.door <- sample(doors[ game != "car" & doors != a.pick ], size = 1)
      }
    return( opened.door )
  }
  
  change_door_extended.2 <- function( stay=T, opened.door, a.pick, door.choices )
  {
    doors <- c(1 : door.choices) 
    
    if( stay )
    {
      final.pick <- a.pick
    } else
      if( ! stay )
      { if(length(doors[doors != opened.door & doors != a.pick]) == 1){
        final.pick <- doors[doors != opened.door & doors != a.pick]
      } else
        final.pick <- sample(doors[ doors != opened.door & doors != a.pick ], size = 1) 
      }
    
    return( final.pick )
  }
  
  determine_winner <- function( final.pick, game )
  {
    if( game[ final.pick ] == "car" )
    {
      return( "WIN" )
    } else
      if( game[ final.pick ] == "goat" )
      {
        return( "LOSE" )
      }
  }
  
  door.choices <- goats + cars
  new.game <- create_game_extended.2(goats, cars)
  first.pick <- select_door_extended.2(door.choices)
  opened.door <- open_goat_door_extended.2( new.game, first.pick, door.choices )
  final.pick.stay <- change_door_extended.2( stay=T, opened.door, first.pick, door.choices )
  final.pick.switch <- change_door_extended.2( stay=F, opened.door, first.pick, door.choices )
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("Stay","Switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}

