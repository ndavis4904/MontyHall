#' Create the orginization of goats and cars.
#' 
#' @param goats Numer of goats that will be hidden behind doors.
#' @param cars Number of cars that will be hidden behind doors.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return create_game_orig() creates a game based on the original Monty Hall problem (2 goats, 1 car).
#' create_game() will allow the user to define the number of goats and cars in the simulation.
#' 
#' @examples
#' ## Randomly sorts 2 goats and 1 car
#' create_game_orig()
#' 
#' ## Randomly sorts 4 goats and 3 cars.
#' create_game(4, 3)
#' 
#' @export
create_game_orig <- function( )
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}

#' @export
create_game <- function( goats, cars )
{
  a.game <- sample( x=c(rep("goat",goats), rep("car", cars)), size = (goats + cars), replace=F )
  return( a.game )
}