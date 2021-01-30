#' Randomly select a door based on the total number of goats and cars.
#' 
#' @param door.choices Total number of doors to choose from.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return door.choices should equal goats + cars from other functions.
#' 
#' @examples 
#' ## Randomly selects doors 1, 2, or 3
#' select_door_orig()
#' 
#' ## Randomly chooses one of the total number of doors.
#' select_door(15)
#' 
#' @export
select_door_orig <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}

#' @export
select_door <- function( door.choices )
{
  doors <- c(1 : door.choices) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )
}