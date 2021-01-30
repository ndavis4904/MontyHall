#' Determines if the player won based on their final chosen door.
#' 
#' @param final.pick The final door chosen.
#' @param game Random selection of goats and cars.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return States if the player has won or lost based on if they landed on the goat or car in the end.
#' 
#' @examples 
#' ## Win based on the original problem.
#' determine_winner(3, c("goat","goat","car"))
#' 
#' ## Determines if the random choices of doors with goats and cars has won or lost.
#' determine_winner(final.pick = 4, create_game(goats = 5, cars = 1))
#' 
#' @export
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