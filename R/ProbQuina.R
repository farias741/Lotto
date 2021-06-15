#' Quina lottery game probability
#'
#'The player simply chooses and dials from 5 to 15 numbers among the 80 available,
#'the draw consists of extracting 5 different numbers, in the universe from 1 to 80.
#'In this game, the winners of 2(duque), 3(terno), 4(quadra) win
#'or 5(quina) numbers.
#'
#'warning message if y (number of hits) is greater than 5, "Prob = 0, the maximum number
#'of hits is 5". However, if y <2, note that this probability does not exist.
#'Otherwise note that the player has won.
#'
#'x is the amount of numbers bet
#'
#'y is the number of hits
#'
#' @param x number
#' @param y number
#'
#' @return number
#' @export
#'
#' @examples ProbQuina(6,7)
#'


ProbQuina = function(x,y){
  prob = (choose(x,y)*choose(80-x,5-y))/choose(80,5)

  if(y>5){
    prob = 0
    message("The maximum number of correct answers is five")
  }

  else if (y<2){
    stop("There is no such probability of hits at Quina")
  }

  else
    message("You won in Quina")



  return(prob)
}

