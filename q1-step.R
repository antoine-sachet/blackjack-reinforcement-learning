# Global values for clearer code
HIT <- 1L; STICK <- 2L

# draw a card , i.e. a value between -10 and 10
drawCard <- function() 
  return(sample(c(-1,1,1),1)*sample(10,1))

# ini returns a valid random initial state
# i.e. an array c(dealerCard, PlayerSum, TerminalState)
s.ini <- function () {
  return ( c(sample(10L, 1), sample(10L, 1), 0L))
} 

# Inputs:
#   s: state, represented by an array of integers
#     DealerCard is the dealer's card (1-10)
#     PlayerSum is the player's sum (1-21)
#     TerminalState is a flag (0 or 1) indicating whether this is a terminal state
#   a: action, represented by 0 (stick) or 1 (hit)
# output:
# list composed of new state and reward
step <- function (s, a) {
  if(s[3]==1)
    return(list(s, as.intger(0)))
  
  new.s <- s
  reward <- 0L
  
  if(a==1) { #HIT
    new.s[2] <- s[2] + drawCard()
    if (new.s[2]>21 || new.s[2]<1) {
      new.s[3] <- 1L
      reward <- -1L
    }
  } else { ##STICK
    new.s[3] <- 1L
    dealerDone <- FALSE
    dealerSum <- s[1]
    while(!dealerDone) {
      dealerSum <- dealerSum + drawCard()
      if (dealerSum>21L || dealerSum<1L) { # DEALER BUSTED
        dealerDone <- TRUE 
        reward <- 1L
      } else if (dealerSum >= 17L) { # DEALER STOP DRAWING
        dealerDone <- TRUE
        if(dealerSum==s[2])
          reward <- 0
        else # reward is -1 if dealerSum>PlayerSum, 0 if equality and +1 otherwise
          reward <- 2L*as.integer(dealerSum<s[2])-1L
      }
    }
  }
  return(list(new.s, reward))
}
