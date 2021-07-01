#' Dia de Sorte lottery game probability
#'
#'The player simply chooses from 7 to 15 numbers among the 31 available, they are extracted
#'7 numbers, you can win by hitting 7, 6, 5 or 4 numbers.
#'
#'warning message if y (number of hits) is greater than 7, "Prob = 0, the maximum number
#'of hits is 7". However, if y <4, note that this probability does not exist.
#'Otherwise note that the player has won.
#'
#'x is the amount of numbers bet ee
#'
#'y is the amount number of hits
#'
#' @param x number
#' @param y number
#'
#' @return number
#' @export
#'
#' @examples ProbDSorte(7,6)


ProbDSorte = function(x,y){
  prob = (choose(x,y)*choose(31-x,7-y))/choose(31,7)

  if(y>7){
    prob = 0
    message("The maximum number of correct answers is seven")
  }

  else if (y<4){
    stop("There is no such probability of hits at Dia de Sorte")

  }

  else
    message("You won in Dia de Sorte")


  return(prob)
}

