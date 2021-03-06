% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/play_game_loop.R
\name{play_game_loop}
\alias{play_game_loop}
\title{Monty Hall game where user defines number of goats and cars which is run N times}
\usage{
play_game_loop(goats, cars, N = 10000)
}
\arguments{
\item{goats}{Numer of goats that will be hidden behind doors.}

\item{cars}{Number of cars that will be hidden behind doors.}

\item{N}{Number of loops of the play_game() function.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
The number of goats added to the number of cars makes the total number of doors to choose from.
N is then how many simulations it will run. N*2 is the number of datapoints in the dataframe.
}
\description{
Monty Hall game where user defines number of goats and cars which is run N times
}
\examples{
## Monty Hall Problem with 2 goats, 1 car, and run 100 times.
play_game(goats = 2, cars = 1, N = 100)

## Monty Hall run with 10 goats, 5 cars, the standard 10,000 times.
play_game(10, 5)

}
