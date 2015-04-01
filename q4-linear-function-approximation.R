######################################################
# File: q4-linear-function-approximation.R
# Author: Antoine Sachet
# Github: github.com/antoine-sachet/easy21-RL-project
# Date: 04/2015
######################################################

# This file contains the functions to perform Temporal Difference RL using linear function approximation.
# I use the SARSA algorithm for on-policy control, implemented using the backward-view (eligibility traces).
#
# The difference with q3 is that the action-value function Q(s,a) is approximated linearly by the function
# f(s,a,w)= t(phi) %*% w,  where w is a vector and phi a vector representing features.
#
# phi(s,a) is a binary feature vector with 3*6*2 = 36 features. Each binary feature
# has a value of 1 if (s,a) lies within the cuboid of state-space corresponding to
# that feature, and the action corresponding to that feature. The cuboids have
# the following overlapping intervals:
#   dealer(s) = [1; 4]; [4; 7]; [7; 10]
#   player(s) = [1; 6]; [4; 9]; [7; 12]; [10; 15]; [13; 18]; [16; 21]
#   a = hit; stick
#
# The helper function epsgreedy is also defined.
#
# Note that some functions from q1-step.R are required.
# Please run q1-step.R prior to running this file.


####### FUNCTION DEFINITIONS #######

if (!exists("HIT")) {
  stop("Please run 'q1-step.R' prior to running this file.")
}

load.library("plyr")
load.library("foreach")
load.library("ggplot2")

# INPUT
#   dealerState: card of the dealer, integer between 1 and 10
# OUTPUT
#   boolean vector coding the dealer card interval on 3 bits
dealerFeatures <- function(dealerState) {
  return (c(
    (dealerState>=1&dealerState<=4),
    (dealerState>=4 & dealerState<=7),
    (dealerState>=7&dealerState<=10)))
}

# INPUT
#   playerState: sum of the player, integer between 1 and 21
# OUTPUT
#   boolean vector coding the player card interval on 6 bits
playerFeatures <- function(playerState) {
  return (c(
    (playerState>=1&playerState<=6),
    (playerState>=4 & playerState<=9),
    (playerState>=7&playerState<=12),
    (playerState>=10 & playerState<=15),
    (playerState>=13&playerState<=18),
    (playerState>=16&playerState<=21)))
}

# INPUT
#   playerState: sum of the player, integer between 1 and 21
# OUTPUT
#   boolean vector coding the player card interval on 6 bits
actionFeatures <- function(action) {
  return (c(action==HIT, action==STICK))
}

# INPUTS
#   s: state (as defined in q1-step.R)
#   a: action, integer: HIT(1) or STICK(2)
# returns a binary vector of length 36 representing the features
phi <- function(s, a) {
  tmp <- array(0, dim=c(3,6,2)) #empty array of dim 3*6*2
  tmp[dealerFeatures(s[1]),
      playerFeatures(s[2]),
      actionFeatures(a)] <- 1 #putting one where a feature is on
  return(as.vector(tmp)) #returning 'vectorized' (1-dim) array
}

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
    return(which.max(c(Q(s,HIT), Q(s,STICK)))) # else action maximizing Q
}

# INPUTS
#   lambda: lambda parameter for SARSA
#       numeric between 0 and 1
#       quantifies the weighting of the steps within an episode
#   gamma: dicounting factor
#       default to 1
#   w: parameter vector of length 36
#     Q(s,a) is approximated by phi*w where phi is detailed above
#   N: counter of state-action visits,
#       that is an array of integers of dim: (10, 21, 2)
#       will be an array of 0 if not provided
#   nb.episode: number of episode to play
#   eps: epsilon for the epsilon-greedy policy
#   step.size: step size for the gradient descent
# OUTPUT
#   list of:
#     w: updated action-value function
#     N: updated counter of state-action visits
sarsa.coarse.coding <- function(lambda, gamma=1, w=NULL, nb.episode=1, eps=0.05, step.size=0.01){
  # setting w and N to their default value if necessary
  if (is.null(w))
    w <- array(0,dim=36)

  # Q is simply the matrix product of phi and w
  Q <- function(s,a) as.vector(phi(s,a) %*% w)

  # defining our policy (reference to Q is not closure)
  policy <- function(s) {
    epsgreedy(s, Q, eps)
  }

  for(i in 1:nb.episode) {
    s <- s.ini()
    # choosing initial action a
    a <- policy(s)
    r <- 0L
    # eligibility trace
    e <- array(0L, dim=36)

    # s[3] is the "terminal state" flag
    # s[3]==1 means the game is over
    while(s[3]==0L) {

      # performing step
      tmp <- step(s, a)
      s2 <- tmp[[1]]
      r <- r + tmp[[2]]

      # if s2 is terminal
      if (s2[3]==0) {
        # choosing new action
        a2 <- policy(s2)
        # sarsa backward view formula with estimated return r+gamma*Q2
        delta <- r + gamma*Q(s2,a2) - Q(s,a)
      } else {
        # sarsa backward view formula, with now known return r
        delta <- r - Q(s,a)
        a2 <- 0L
      }
      ind <- which(e>0)
      # updating eligibility traces and w
      e <- gamma*lambda*e + phi(s,a)
      w <- w + step.size*delta*e
      s <- s2
      a <- a2
    }
  }
  return(w)
}

######## COMPUTING w WITH SARSA COARSE CODING #########

# loading results from montecarlo
if(!exists("res")){ # if q2-montecarlo-control.R was executed, there is no need to load anything
  load("D:/Sachou/UCL/RL/easy21-RL-project/results/MonteCarlo/q2-MC-res-4Mepi.Robj")
}
QMC <- res$Q

MSE <- foreach(1:10, .combine='rbind') %do% {
  # computing w for lambda from 0 to 1
  lambdas <- seq(0, 1, 0.1)
  W <- foreach(lambda=lambdas) %do% {
    sarsa.coarse.coding(lambda, nb.episode=1000)
  }
  # enumerating all possible states
  states <- expand.grid(dealer=1:10, player=1:21, action=c(HIT, STICK))
  # computing MSE approximated Q and montecarlo's Q
  foreach(w=W, .combine=c) %do% {
    # computing squared errors w.r.t montecarlo's action-value function
    sqdiff <- foreach(s=states, .combine=c) %do% {
      (phi(c(s[1], s[2]),s[3])%*%w - QMC[s])**2
    }
    mean(sqdiff) # taking the mean of the SE
  }
}
mse <- colMeans(MSE)
# plotting the MSE vs lambda
data <- data.frame(lambda=lambdas, mse=mse, se=aaply(MSE, .margins=2, sd))
ggplot(data, aes(x=lambdas))+geom_point(aes(y=mse))+geom_errorbar(aes(ymax=mse+se, ymin=mse-se))

# computing MSE at every step for lambda=1
w=NULL
mse <- times(1000) %do% {
  w <- sarsa.coarse.coding(lambda=1, w=w, nb.episode=1)
  sqdiff <- laply(states, .fun=function(s) {
    (phi(c(s[1], s[2]),s[3])%*%w - QMC[s])**2
  })
  mean(sqdiff) # taking the mean of the SE
}
plot(mse)

# computing MSE at every step for lambda=0
w=NULL
mse <- times(1000) %do% {
  w <- sarsa.coarse.coding(lambda=0, w=w, nb.episode=1)
  sqdiff <- laply(states, .fun=function(s) {
    (phi(c(s[1], s[2]),s[3])%*%w - QMC[s])**2
  })
  mean(sqdiff) # taking the mean of the SE
}
plot(mse)
