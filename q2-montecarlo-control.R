library("plyr") # for aaply

if (!exists("HIT")) {
  # should set working directory to project folder, e.g.
  # setwd("D:/Sachou/UCL/RL/easy21-RL-project")
  source("q1-step.R")
}

epsgreedy <- function(s, Q, eps) {
  if(runif(1)<eps)
    return(sample(1:2, 1))
  else
    return(which.max(Q[s[1],s[2],]))
}

montecarlo <- function(Q=NULL, N=NULL, nb.episode=1, N0=100){
  if (is.null(Q))
    Q <- array(0, dim=c(10, 21, 2))

  if (is.null(N))
    N <- array(0L, c(10,21,2))

  foreach(i=1:nb.episode) %do% {
    # defining policy for new Q
    policy <- function(s) {
      epsgreedy(s, Q, N0/(sum(N[s[1], s[2],])+N0))
    }
    s <- s.ini()
    r <- 0L
    # counter for state visits during this episode
    N.episode <- array(0L, c(10,21,2))

    # s[3] is a flag indicating the terminal state
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
persp(V, theta=55, phi=40, d=1.5, expand=0.6, border=NULL, ticktype="detailed",
           shade=0.5, xlab="Dealer showing", ylab="Player sum", zlab="Value", nticks=c(10,21,10))

# performing additional episodes
res <- montecarlo(N=res$N, Q=res$Q, nb.episode=100000)

# plotting decision heatmap (HIT in red and STICK in blue)
astar <- aaply(res$Q, .margins=c(1,2), .fun=which.max)
heatmap(astar, Rowv=NA, Colv=NA, col=c(rep("red",5), rep("blue",2)))
