#'  LotoMania lottery game probability
#'
#'The player only has to choose 50 numbers out of 100 available,
#'the draw consists of extracting 20 different numbers, between 00 and 99.
#'In this game, winners of 20, 19, 18, 17, 16, 15 or those who do not match
#'any number win prizes.
#'
#'x the amount of numbers bet
#'
#'y the amount number of hits
#'
#'
#' @param x number
#' @param y number
#'
#' @return number
#' @export
#'
#' @examples ProbLMania(50,20)


ProbLMania = function(x,y){
  prob = (choose(x,y)*choose(100-x,20-y))/choose(100,20)

  if(y>20){
    prob = 0
    message("The maximum number of correct answers is 20")
  }

  else if (y<15){
    message("There is no such probability of hits at LotoMania")

  }

  else
    message("You won in LotoMania")

  return(prob)
}
