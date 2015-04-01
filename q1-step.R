########################
# File: q1-step.R
# Author: Antoine Sachet
# Github: github.com/antoine-sachet/easy21-RL-project
# Date: 04/2015
########################

# This file contains the main game function: step(state, action)
# as well as some helper functions: drawCard() and s.ini()
#
# In order to make the code plug and play, a custom function load.library
# is also defined to load a library, installing the lib if it is missing.
#

# try to load a library and install the missing lib if needed
load.library <- function(package_name) {
  package_name <- as.character(match.call()[[2]])
  if(!require(package_name, character.only=T)){
    install.packages(package_name)
    require(package_name)
  }
}

# plyr is a lib with convenient "apply" functions
load.library("plyr")

# Global values for clearer code
HIT <- 1L; STICK <- 2L

# function to draw a card , i.e. a value between -10 and 10
drawCard <- function()
  return(sample(c(-1,1,1),1)*sample(10,1))

# function s.ini returns a valid random initial state
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
# list of:
#   new state
#   reward
step <- function (s, a) {
  # if terminal state, exit gracefully
  if(s[3]==1)
    return(list(s, as.integer(0)))

  # both new state and reward will be returned
  new.s <- s
  reward <- 0L

  if(a==1) { # if HIT
    new.s[2] <- s[2] + drawCard() # drawing card
    if (new.s[2]>21 || new.s[2]<1) { #checking if busted
      new.s[3] <- 1L
      reward <- -1L
    }
  } else { ##if STICK
    new.s[3] <- 1L # setting 'terminal state' flag
    dealerDone <- FALSE
    dealerSum <- s[1]
    while(!dealerDone) { # dealer draws until 17 or busted
      dealerSum <- dealerSum + drawCard()
      if (dealerSum>21L || dealerSum<1L) { # DEALER BUSTED
        dealerDone <- TRUE
        reward <- 1L
      } else if (dealerSum >= 17L) { # DEALER STOP DRAWING
        dealerDone <- TRUE
        if(dealerSum==s[2]) # reward is 0 if equality
          reward <- 0
        else # reward is -1 if dealerSum>PlayerSum, +1 otherwise
          reward <- 2L*as.integer(dealerSum<s[2])-1L
      }
    }
  }
  # returning new state and reward
  return(list(new.s, reward))
}
