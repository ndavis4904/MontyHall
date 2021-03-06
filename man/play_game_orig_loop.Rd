% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/play_game_orig_loop.R
\name{play_game_orig_loop}
\alias{play_game_orig_loop}
\title{The original Monty Hall game run N times}
\usage{
play_game_orig_loop(N = 10000)
}
\arguments{
\item{N}{Number of loops of the play_game() function.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
The number of goats added to the number of cars makes the total number of doors to choose from.
N is then how many simulations it will run. N*2 is the number of datapoints in the dataframe.
}
\description{
The original Monty Hall game run N times
}
\examples{
## Monty Hall Problem with 2 goats, 1 car, and run 100 times.
play_game(N = 100)

## Monty Hall run the standard 10,000 times.
play_game()

}
