#' Mega Sena lottery game probability
#'
#'The bettor simply chooses and dials 6 to 15 numbers from 60 available,
#'the draw consists of the extraction of 6 different numbers, in the universe from 1 to 60.
#'In that game, winners of 4,5 or 6 numbers win prizes.
#'
#'warning message if the y (number of hits) is greater than 6, "Prob = 0, the maximum number
#'of hits is 6". However if y <= 3, notice that there is no such probability.
#'Otherwise, notice that the bettor has won.
#'
#'x the amount of numbers bet
#'
#'y the amount number of hits
#'
#' eu sou debora
#' @param x number
#' @param y number
#'
#' @return number
#' @export
#'
#' @examples ProbMSena(6,9)


ProbMSena = function(x,y){


  prob = (choose(x,y)*choose(60-x,6-y))/choose(60,6)

  if(y>6){
    prob = 0
    message("The maximum number of correct answers is 6")
  }

  else if (y<=3){
    message("There is no such probability of hits at MegaSena")
  }

  else
    message("You won in MegaSena")


  return(prob)
}

