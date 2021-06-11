#' Loto Facil lottery game probability
#'
#'The player simply chooses and dials between 15 and 20 numbers, among the 25 available,
#'the draw consists of extracting 15 different numbers, in the universe from 1 to 25. In this game,
#'the prizes are awarded to the winners of 11, 12, 13, 14 or 15 numbers.
#'
#warning message if the y (number of hits) is greater than 15, "Prob = 0, the maximum number
#'of hits is 15". However if y < 11, notice that there is no such probability.
#'Otherwise, notice that the bettor has won.
#'
#'x the amount of numbers bet
#'
#'y the amount number of hits
#'
#' @param x number
#' @param y number
#'
#' @return number
#' @export
#'
#' @examples ProbLFacil(15,10)



ProbLFacil = function(x,y){
  prob = (choose(x,y)*choose(25-x,15-y))/choose(25,15)

  if(y>15){
    prob = 0
    message("The maximum number of correct answers is 15")
  }

  else if (y<11){
    message("There is no such probability of hits at LotoFacil")

  }

  else
    message("You won in LotoFacil")

  return(prob)
}


