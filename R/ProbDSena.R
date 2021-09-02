#' Dupla Sena lottery game probability
#'
#' Does the ticket have 50 tens, in which the bettor can? dial from 6 to 15 numbers,
#' consists of extracting 6 different numbers, you can win by hitting 3, 4, 5 or
#' 6 numbers, with a differential, the player has two chances to win.
#'
#' To choose 6 numbers, the bet is simple, where it offers the lowest odds
#' win: combination of 50 numbers taken from 6 to 6 = 50!/6!44! = 15890700
#' However, as there are two draws, the player has two chances out of approximately 16 million.
#' 2/15890700 = 1/7945350, to win any of the SENAS, the player has approximately 1 chance in 8 million.
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
#' @examples ProbDSena(15,5)


ProbDSena = function(x,y){

  prob = (choose(x,y)*choose(50-x,6-y)*2)/choose(50,6)

  if(y>6){
    prob = 0
    message("The maximum number of correct answers is six")
  }


  else if(y<=2){
    stop("There is no such probability of hits in Dupla Sena")

  }

  else
    if(y==3)
      message("Happy birthday. you hit the Terno!")
  if(y==4)
    message("Happy birthday. you hit the Quadra!")
  if(y==5)
    message("Happy birthday. you hit the Quina!")
  if(y==6)
    message("Happy birthday. You are champion of the Dupla Sena!")

  return(prob)
}
