#' @title Create a new Monty Hall Problem game
#'
#' @description
#' `create_game()` generates a new game that consists of two doors
#' with goats behind them and one door with a car.
#'
#' @details
#' The game setup replicates the TV show "Let's Make a Deal". One car
#' and two goats are randomly assigned to three doors. The contestant
#' initially selects a door, after which the host reveals a goat behind
#' one of the remaining doors.
#'
#' @return
#' A character vector of length three indicating the placement of
#' two goats and one car.
#'
#' @examples
#' create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample(c("goat","goat","car"), size = 3, replace = FALSE)
  return(a.game)
}



#' @title Randomly select a door
#'
#' @description
#' Randomly selects one of the three doors for the contestant.
#'
#' @details
#' The contestant has no information about what is behind each door,
#' so the selection is made uniformly at random.
#'
#' @return
#' An integer value of 1, 2, or 3 indicating the selected door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function()
{
  doors <- c(1,2,3)
  a.pick <- sample(doors, size = 1)
  return(a.pick)
}



#' @title Open a door revealing a goat
#'
#' @description
#' Opens a door that reveals a goat and is not the contestant's pick.
#'
#' @details
#' If the contestant initially selects the car, the host randomly opens
#' one of the remaining goat doors. If the contestant selects a goat,
#' the host opens the only remaining goat door.
#'
#' @param game
#' A character vector representing the game setup.
#'
#' @param a.pick
#' An integer indicating the contestant's initial door choice.
#'
#' @return
#' An integer indicating which door the host opens.
#'
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' open_goat_door(game, pick)
#'
#' @export
open_goat_door <- function(game, a.pick)
{
  doors <- c(1,2,3)

  if (game[a.pick] == "car")
  {
    goat.doors <- doors[game != "car"]
    opened.door <- sample(goat.doors, size = 1)
  }

  if (game[a.pick] == "goat")
  {
    opened.door <- doors[game != "car" & doors != a.pick]
  }

  return(opened.door)
}



#' @title Stay with or change the selected door
#'
#' @description
#' Determines the contestant's final door choice based on strategy.
#'
#' @details
#' If `stay = TRUE`, the contestant keeps the original door. If
#' `stay = FALSE`, the contestant switches to the remaining unopened door.
#'
#' @param stay
#' Logical value indicating whether the contestant stays with
#' the original choice.
#'
#' @param opened.door
#' The door opened by the host.
#'
#' @param a.pick
#' The contestant's original door selection.
#'
#' @return
#' An integer indicating the contestant's final door choice.
#'
#' @examples
#' change_door(TRUE, opened.door = 2, a.pick = 1)
#' change_door(FALSE, opened.door = 2, a.pick = 1)
#'
#' @export
change_door <- function(stay = TRUE, opened.door, a.pick)
{
  doors <- c(1,2,3)

  if (stay)
  {
    final.pick <- a.pick
  }

  if (!stay)
  {
    final.pick <- doors[doors != opened.door & doors != a.pick]
  }

  return(final.pick)
}



#' @title Determine whether the contestant wins
#'
#' @description
#' Determines if the contestant wins the game by selecting the car.
#'
#' @details
#' A win occurs when the final door choice contains the car.
#'
#' @param final.pick
#' The contestant's final door selection.
#'
#' @param game
#' The game configuration vector.
#'
#' @return
#' A character string: "WIN" if the car is selected, otherwise "LOSE".
#'
#' @examples
#' game <- c("goat","car","goat")
#' determine_winner(2, game)
#'
#' @export
determine_winner <- function(final.pick, game)
{
  if (game[final.pick] == "car")
  {
    return("WIN")
  }

  if (game[final.pick] == "goat")
  {
    return("LOSE")
  }
}



#' @title Play one Monty Hall game
#'
#' @description
#' Simulates a single Monty Hall game under both strategies.
#'
#' @details
#' The function plays one game and records the outcome for staying
#' and switching strategies.
#'
#' @return
#' A data frame with the strategy used and the outcome.
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function()
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door(new.game, first.pick)

  final.pick.stay <- change_door(TRUE, opened.door, first.pick)
  final.pick.switch <- change_door(FALSE, opened.door, first.pick)

  outcome.stay <- determine_winner(final.pick.stay, new.game)
  outcome.switch <- determine_winner(final.pick.switch, new.game)

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay, outcome.switch)

  game.results <- data.frame(strategy, outcome, stringsAsFactors = FALSE)
  return(game.results)
}



#' @title Play multiple Monty Hall games
#'
#' @description
#' Simulates many Monty Hall games and summarizes the results.
#'
#' @details
#' The function repeats the Monty Hall simulation `n` times and prints
#' the proportion of wins and losses for each strategy.
#'
#' @param n
#' Number of games to simulate.
#'
#' @return
#' A data frame containing the results of all simulated games.
#'
#' @examples
#' play_n_games(10)
#'
#' @export
play_n_games <- function(n = 100)
{
  results.list <- list()
  loop.count <- 1

  for (i in 1:n)
  {
    game.outcome <- play_game()
    results.list[[loop.count]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows(results.list)

  table(results.df) |>
    prop.table(margin = 1) |>
    round(2) |>
    print()

  return(results.df)
}