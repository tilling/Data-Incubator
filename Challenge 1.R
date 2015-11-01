#Code for challenge problem 1


# There are 2 approaches: simulation, and Markov.
# For each of them, they are done in 1 or 2 dimensions.
# Dimension 1 will be coded first because it's so
# much simpler.

# Simulation doesn't require the same cleverness
# as the Markov version at keeping track of the
# system state. You just take a function of the
# simulated path.

Sim1D <- function(n.sim, n.moves, fn.path) {
  #n.sim is number of simulations
  #n.moves is number of moves per simulation
  
  sim <- rep(NA, n.sim)
  for (i.sim in 1:n.sim) {
    which.move <- rbinom(n.moves, size=1, prob=0.5)
    move <- (-1)^which.move
    path <- c(0, cumsum(move))
    sim[i.sim] <- fn.path(path)
  }
  return(mean(sim, na.rm=TRUE))
}

#To use Sim1D for different questions:

#ever.above returns a function of x.
#That function is:
#true if max(x) >= M
#false otherwise.
ever.above <- function(M) {
  function(x) max(x) >= M
}

#This function is different. The function it returns
#is true if the FINAL position is at most lo,
#but the MAX was at least hi.
ever.above.now.below <- function(lo, hi) {
  function(x) {
    x[length(x)] <= lo & ever.above(hi)(x)
  }
}

East.West.10.sim <- Sim1D(1e5, 10, ever.above.now.below(-2, 2))
East.West.30.sim <- Sim1D(1e5, 30, ever.above.now.below(-2, 2))





#The following are for Markov chains in 1D.
adjust.vector <- function(x, y, shift=0, mult=1) {
  #Adjust the first n-shift entries of x by
  #a multiplier of the rightmost n-shift entries
  #of y
  n <- length(x) #it had better be length(y) too
  base.range <- 1:(n - abs(shift))
  if (shift >= 0) {
    x.inds <- base.range
    y.inds <- x.inds + shift
  } else {
    y.inds <- base.range
    x.inds <- y.inds - shift
  }
  x[x.inds] <- x[x.inds] + mult * y[y.inds]
  return(x)
}

#Simpler Markov: just one state!
Markov.Position.1D <- function(n.max, n.moves) {
  if (n.moves > n.max) {
    print("More moves than positions allow; increasing positions")
    n.max <- n.moves
  }
  n.points = 2*n.max + 1;
  ctr = n.max + 1; #the index for "0"
  p0 = rep(0, n.points)
  p0[ctr] = 1
  
  for (i.move in 1:n.moves) {
    p1 <- rep(0, n.points)
    p1 <- adjust.vector(p1, p0, 1, 0.5)
    p1 <- adjust.vector(p1, p0, -1, 0.5)
    p0 <- p1
  }
  return(p1)
}


#Simpler Markov: just one state!
Markov.Ever.1D <- function(n.max, n.moves, bound) {
  #Now the question is, for each point x,
  #how likely is it that the visitor was
  #EVER at position at least ctr + bound
  #e.g. at ctr+1 or to the right of it if
  #bound==1.
  n.points = 2*n.max + 1;
  ctr = n.max + 1; #the index for "0"
  p0.ever = rep(0, n.points)
  p0.total = rep(0, n.points)
  p0.total[ctr] <- 1
  p0.ever[(ctr + bound):n.points] <- p0.total[(ctr + bound):n.points]

  
  #It will help us to have the TOTAL probability
  #of being at position x
  
  for (i.move in 1:n.moves) {
    p1.ever <- rep(0, n.points)
    #if it is AT or ABOVE ctr+bound, then it is certain to
    #have "ever"
    #however, that is not the only way it can happen!
    p1.ever <- adjust.vector(p1.ever, p0.ever, 1, 0.5)
    p1.ever <- adjust.vector(p1.ever, p0.ever, -1, 0.5)
    
    p1.total <- Markov.Position.1D(n.max, i.move)
    p1.ever[(ctr + bound):n.points] <- p1.total[(ctr + bound):n.points]    
    p0.ever <- p1.ever
  }
  return(p1.ever)
}

Markov.Above.Below <- function (n.max, lo, hi) {
  prob.ever <- Markov.Ever.1D(n.max, n.max, hi)
  ctr <- n.max + 1
  range <- 1:(ctr + lo)
  return( sum(prob.ever[range]))
}

East.West.10.true <- Markov.Above.Below(10, -2, 2)
East.West.30.true <- Markov.Above.Below(30, -2, 2)

Markov1D <- function(n.moves, is.true) {
  #is.true is a function of the current and previous
  #point.
  #For each move, p is the probability of 
  #the proposition being true. The real states at
  #each point i is (at i, TRUE) and (at i, FALSE).
  
  n.points = 2*n.moves + 1;
  ctr = n.moves + 1; #the index for "0"
  markov.probs <- vector(n.moves, mode='list')
  #initialize
  
  markov.probs[[1]] <- list(
    true = rep(0, n.points)
    #false = rep(0, n.points) #EXCEPT
    #false[ctr] <- 1
    #sum(true) + sum(false) must = 1
    )
  
  #Make simple index vectors for left and right--
  #you'll use them!
  left <- 1:(n.points - 1)
  shift.left <- 1
  shift.right <- 2
  was.true <- 1
  was.false <- 2
  right <- 2:n.points
  
  for(i.moves in 2:n.moves) {
    last.probs <- markov.probs[[i.moves - 1]]
    new.probs$true <- rep(0, n.points)
    new.probs$true <- adjust.vector(new.probs$true,
                          last.probs$true, 1, 0.5*is.true[shift.left, was.true])
    new.probs$true <- adjust.vector(new.probs$true,
                          last.probs$false, -1, 0.5*is.true[shift.right, was.true])
  
    new.probs$false <- rep(0, n.points)
    new.probs$false <- adjust.vector(new.probs$true,
                           last.probs$true, 1, 0.5*is.true[shift.left, was.true])
    new.probs$false <- adjust.vector(new.probs$true,
                           last.probs$true, -1, 0.5*is.true[shift.right, was.true])
  }
}

