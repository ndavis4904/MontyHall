#' Randomly chooses one of the doors with a goat that is not the selected door
#' 
#' @param game Random assortment of goats and cars
#' @param a.pick The player selected door
#' @param door.choices Number of total possible doors
#' @param ... further arguments passed to or from other methods.
#' 
#' @return This function will open a random door that is not the door the player has selected as well as one that has a goat hidden behind.
#' 
#' @examples 
#' ## Opens a single door between 1 and 3 from the original problem
#' open_goat_door_orig(create_game_orig(), 2)
#' 
#' ## Randomly selects 1 door out of any remaining doors that fit the above condition.
#' open_goat_door(create_game(5, 3), 2, 8) 
#' 
#' @export
open_goat_door_orig <- function( game, a.pick )
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

#' @export
open_goat_door <- function( game, a.pick, door.choices )
{
  doors <- c(1 : door.choices)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  } else
    if( game[ a.pick ] == "goat" )
    { 
      opened.door <- sample(doors[ game != "car" & doors != a.pick ], size = 1)
    }
  return( opened.door ) # number between 1 and 3
}