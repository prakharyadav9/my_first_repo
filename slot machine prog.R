##slot machine game
wheel <- c("DD","7","BBB","BB","B","C","0" )
##func 1:

get_symbols <- function(){
  sample(wheel,size = 3,replace = T,prob = c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}
##play function : main function
play <- function(){
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols()
  prize
}

# prog defn start:

score <- function(symbols){
  #identify case
same <- symbols[1]==symbols[2]&& symbols[2]==symbols[3]
bars <- (symbols %in% c("B","BBB","BB"))
##payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,  "B" = 10, 
             #"C" = 10, "0" = 0) 
##parallel cases codes
if(same){
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,  
               "B" = 10,"C" = 10, "0" = 0)
  prize= unname(payouts[symbols[1]])
}else if(all (bars)){
  prize <- 5
}else {
  cherries <- sum(symbols=="C") #count cherries
  prize <- c(0,2,5)[cherries+1]
}
diamonds <- sum(symbols=="DD")
prize * 2^diamonds
}
play()


