#####################################################
# File: q3-TD-learning.R
# Author: Antoine Sachet
# Github: github.com/antoine-sachet/easy21-RL-project
# Date: 04/2015
#####################################################

# This file contains the functions to perform Temporal Difference reinforcement learning.
# I use the SARSA algorithm for on-policy control,
# implemented using the backward-view (eligibility traces).
#
# The helper function epsgreedy is also defined.
#
# Note that the function sarsa() requires some functions from q1-step.R.
# Please run q1-step.R prior to running this file.


####### FUNCTION DEFINITIONS #######

if (!exists("HIT")) {
  tryCatch(source("q1-step.R"),
           warning=function(e) {
             stop("Please run 'q1-step.R' prior to running this file.")
           })
}

load.library("plyr")
load.library("foreach")

# INPUTS
#   s: state (as defined in q1-step.R)
#   Q: action-value function, that is an array of dim: (10, 21, 2)
#   eps: numeric value for epsilon
# OUTPUT
# action to take following an epsilong-greedy policy
# 1 is HIT and 2 is STICK (as defined in q1-step.R)
epsgreedy <- function(s, Q, eps) {
  if(runif(1)<eps)
    return(sample(1:2, 1)) # random action taken with eps probability
  else
    return(which.max(Q[s[1],s[2],])) # else action maximizing Q
}

# INPUTS
#   lambda: lambda parameter for SARSA
#       numeric between 0 and 1
#       quantifies the weighting of the steps within an episode
#   gamma: dicounting factor
#       default to 1
#   Q: action-value "function",
#       that is an array of dim: (10, 21, 2)
#       will be an array of 0 if not provided
#   N: counter of state-action visits,
#       that is an array of integers of dim: (10, 21, 2)
#       will be an array of 0 if not provided
#   nb.episode: number of episode to play
#   N0: offset for N.
#       At each episode, an epsilon-greedy action is taken with
#       eps = N0/(total+N0) where total is the total number of times
#       this state has been visited.
# OUTPUT
#   list of:
#     Q: updated action-value function
#     N: updated counter of state-action visits
sarsa <- function(lambda, gamma=1, Q=NULL, N=NULL, nb.episode=1, N0=100){
  # setting Q and N to their default value if necessary
  if (is.null(Q))
    Q <- array(0, dim=c(10, 21, 2))
  if (is.null(N))
    N <- array(0L, c(10,21,2))

  # defining our policy (reference to Q is not closure)
  policy <- function(s) {
    epsgreedy(s, Q, N0/(sum(N[s[1], s[2],])+N0))
  }

  for(i in 1:nb.episode) {
    s <- s.ini()
    # choosing initial action a
    a <- policy(s)
    r <- 0L
    # eligibility trace
    e <- array(0L, c(10,21,2))

    # s[3] is the "terminal state" flag
    # s[3]==1 means the game is over
    while(s[3]==0L) {

      # incrementing the visit counter
      N[s[1], s[2], a] <- N[s[1], s[2], a] + 1
      # incrementing eligibility trace
      e[s[1], s[2], a] <- e[s[1], s[2], a] + 1
      # performing step
      tmp <- step(s, a)
      s2 <- tmp[[1]]
      r <- r + tmp[[2]]

      # if s2 is terminal
      if (s2[3]==0) {
        # choosing new action
        a2 <- policy(s2)
        # sarsa backward view formula with estimated return r+gamma*Q2
        delta <- r + gamma*Q[s2[1],s2[2],a2] - Q[s[1],s[2],a]
      } else {
        # sarsa backward view formula, with now known return r
        delta <- r - Q[s[1],s[2],a]
        a2 <- 0L
      }
      ind <- which(e>0)
      # updating Q
      Q[ind] <- Q[ind] + delta*e[ind]/N[ind] # alpha=1/N
      e <- gamma*lambda*e
      s <- s2
      a <- a2
    }
  }
  return(list(Q=Q,N=N))
}


######## COMPUTING Q WITH SARSA #########

# loading results from montecarlo
if(!exists("res")){ # if q2-montecarlo-control.R was executed, there is no need to load anything
  warning(paste("Results from q2-montecarlo-control.R not found in memory.",
    "Will load results from results/MonteCarlo/q2-MC-res-4Mepi.Robj"))
  load("results/MonteCarlo/q2-MC-res-4Mepi.Robj")
}
QMC <- res$Q

# computing Q for lambda from 0 to 1
lambdas <- seq(0, 1, 0.1)
Qsarsa <- llply(lambdas, .fun= function(lambda) {
  sarsa(lambda, nb.episode=1000)[[1]]
})

# computing MSE between sarsa's Q and montecarlo's Q
MSE <- laply(Qsarsa, .fun=function(Q) {
  mean((Q-QMC)**2)
})

# plotting the MSE vs lambda
plot(lambdas, MSE)

# computing MSE at every step for lambda=1
Q <- NULL
N <- NULL
mse <- times(1000) %do% {
  res <- sarsa(lambda=1, Q=Q, N=N, nb.episode=1)
  Q <- res$Q; N <- res$N
  mean((Q-QMC)**2)
}
plot(mse)

# computing MSE at every step for lambda=0
Q <- NULL
N <- NULL
mse <- times(1000) %do% {
  res <- sarsa(lambda=0, Q=Q, N=N, nb.episode=1)
  Q <- res$Q; N <- res$N
  mean((Q-QMC)**2)
}
plot(mse)
