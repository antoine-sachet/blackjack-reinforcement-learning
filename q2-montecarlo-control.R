#####################################################
# File: q2-montecarlo-control.R
# Author: Antoine Sachet
# Github: github.com/antoine-sachet/easy21-RL-project
# Date: 04/2015
#####################################################

# This file contains the main function to perform Monte-Carlo reinforcement learning.
# The helper function epsgreedy is also defined.
#
# Note that the function montecarlo requires some functions from q1-step.R.
# Please run q1-step.R prior to running this file.

if (!exists("HIT")) {
  stop("Please run 'q1-step.R' prior to running this file.")
}

load.library("plyr")

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
montecarlo <- function(Q=NULL, N=NULL, nb.episode=1, N0=100){
  # setting Q and N to their default value if necessary
  if (is.null(Q))
    Q <- array(0, dim=c(10, 21, 2))
  if (is.null(N))
    N <- array(0L, c(10,21,2))

  # defining our policy
  policy <- function(s) {
    epsgreedy(s, Q, N0/(sum(N[s[1], s[2],])+N0))
  }

  foreach(i=1:nb.episode) %do% {
    s <- s.ini()
    r <- 0L
    # counter for state visits during *this* episode
    N.episode <- array(0L, c(10,21,2))

    # s[3] is the "terminal state" flag
    # s[3]==1 means the game is over
    while(s[3]==0L) {
      # choosing action a
      a <- policy(s)
      # incrementing the visit counter
      N.episode[s[1], s[2], a] <- N.episode[s[1], s[2], a] + 1
      # performing step
      tmp <- step(s, a)
      s <- tmp[[1]]
      r <- r + tmp[[2]]
    }
    # updating Q and N
    ind <- which(N.episode!=0)
    N[ind] <- (N[ind]+N.episode[ind])
    Q[ind] <- Q[ind] + (r-Q[ind]) / N[ind]
  }
  return(list(Q=Q, N=N))
}

# computing MC with nb.episode = number of episodes
res <- montecarlo(nb.episode=100000)

# computing and plotting value function
V <- aaply(res$Q, .margins=c(1,2), .fun=max)
persp(V, x=1:10, y=1:21, theta=55, phi=40, d=1.5, expand=0.6, border=NULL, ticktype="detailed",
      shade=0.5, xlab="Dealer showing", ylab="Player sum", zlab="Value", nticks=10)

# performing additional episodes
res <- montecarlo(N=res$N, Q=res$Q, nb.episode=100000)

# plotting decision heatmap (HIT in red and STICK in blue)
astar <- aaply(res$Q, .margins=c(1,2), .fun=which.max)
heatmap(astar, Rowv=NA, Colv=NA, col=c(rep("red",5), rep("blue",2)))
