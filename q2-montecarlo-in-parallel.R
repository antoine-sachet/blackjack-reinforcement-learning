library("plyr") # for aaply

if (!exists("HIT")) {
  setwd("D:/Sachou/UCL/RL")
  source("q1-efficient.R")
}

if (!exists("clusters")) {
  library("doParallel")
  clusters <- makeCluster(4)
  registerDoParallel(clusters)
}

epsgreedy <- function(s, Q, eps) {
  if(runif(1)<eps)
    return(sample(1:2, 1))
  else
    return(which.max(Q[s[1],s[2],]))
}

# res is a list composed of Q and N
episode.combine <- function (res, episode.res) {
  new.res <- res
  ind <- which(episode.res$N!=0)
  new.res$Q[ind] <- (res$N[ind] * res$Q[ind] + episode.res$N[ind] * episode.res$r) / (res$N[ind]+episode.res$N[ind])
  new.res$N[ind] <- (res$N[ind]+episode.res$N[ind])
  return(new.res)
}

montecarlo <- function(Q=NULL, N=NULL, nb.episode=1, N0=100){
  if (is.null(Q))
    Q <- array(0, dim=c(10, 21, 2))
  
  if (is.null(N))
    N <- array(0L, c(10,21,2))
  
  foreach(i=1:round(nb.episode/10)) %do% {
    res <- foreach(j=1:10, .combine=episode.combine, .init=list(Q=Q,N=N)) %dopar% {
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
      list(r=r, N=N.episode)
    }
    Q <- res$Q
    N <- res$N
  }
  return(list(Q=Q, N=N))
}

res <- montecarlo(res$Q, res$N, nb.episode=100000)
V <- aaply(res$Q, .margins=c(1,2), .fun=max)
astar <- aaply(res$Q, .margins=c(1,2), .fun=which.max)
heatmap(astar, Rowv=NA, Colv=NA)
p <- persp(V, theta=35, phi=30, d=1.5, expand=0.4, border=NULL, ticktype="detailed", 
           shade=0.5, xlab="Dealer showing", ylab="Player sum", zlab="Value", nticks=c(10,21,10))
res$Q
