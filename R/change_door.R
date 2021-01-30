#' Gives the player the opion to switch to a different door or to stay with the original door.
#' 
#' @param stay = T True means that the player would stay with the original door, False would mean to switch to a different door
#' @param opened.door The door that has been opened with a goat behind it.
#' @param a.pick The original door selected by the player
#' @param door.choices The total number of doors in the game.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return This function allows the player to choose if they would like to stay on their original door or switch their choice to a different door given the results of the opened door.
#' 
#' @examples 
#' ## Player switches the door to the only remaining door.
#' change_door_orig(F, 2, 1)
#' 
#' ## Randomly chooses a remaining door from those that remain.
#' change_door(F, opened.ddor = 3, a.pick = 2, door.choices = 8)
#' 
#' @export
change_door_orig <- function( stay=T, opened.door, a.pick )
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

#' @export
change_door <- function( stay=T, opened.door, a.pick, door.choices )
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
  
  return( final.pick )  # number between 1 and 3
}