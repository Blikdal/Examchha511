#' @export
#' @author Christian Blikdal Hansen
#' @title stationarydist
#' @keywords Stationary_Distibution Transition_Probability_Matrix Simulation
#' @return Transition distribution for each state
#' @example KDE(A, 3, 10000)
#' @param p <- transition probability matrix
#' @param k <- initial state
#' @param n <- number of simulated steps
# from the information in the diagram, I make the following matrix, A:
# the function has to take 3 things into consideration: n, k and p
#n <- 10000 # is the number of simulations
#k <-  is the initial step
#p <-  is the probability from the diagram, transformed into matrix, A:

stationarydist <-function(p, k, n){
  simstate <- c()
  #this is the stationary distrbution, that we want to simulate, the probabily to be instate i={1,...,6}
  for(i in 1:dim(A)[1]){
    simstate <- append(simstate, 0)
  }
  state <- k
  for(i in 1:n){
    probability=0
    U <- runif(1)
    for(i in 1:dim(p)){
      if(U <= p[state,i]+probability && U > probability){
        state=i
        simstate[i] <- simstate[i]+1
        break
      }
      probability <- probability+p[state,i]
    }
  }
  return(simstate/n)
}

