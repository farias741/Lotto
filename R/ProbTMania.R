#' Timemania lottery game probability
#'
#'The player chooses 10 numbers from the 80 available, 7 numbers are extracted,
#'then to compete for the prizes for hits of 7, 6, 5, 4, 3 and the team of heart.
#'
#'Warning message if y (number of hits) is greater than 7, "Prob = 0, the maximum number
#'of hits is 7". However, if y <3, note that this probability does not exist.
#'Otherwise note that the player has won.
#'
#'x is the amount of numbers bet
#'
#'y is the amount number of hits
#'
#' @param x number
#' @param y number
#'
#' @return number
#' @export
#'
#' @examples ProbTMania(10,9)


ProbTMania = function(x,y){
  prob = (choose(x,y)*choose(80-x,7-y))/choose(80,7)

  if(y>7){
    prob = 0
    message("The maximum number of correct answers is seven")
  }

  else if (y<3){
    stop("There is no such probability of hits at Timemania")

  }

  else
    message("You won in Timemania")

  return(prob)
}
