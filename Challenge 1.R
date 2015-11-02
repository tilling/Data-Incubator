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



#Now you have to re-create that general setup in 2D.
#Never used Markov1D (I used Position.1D and Ever.1D instead)
#so it's gone.

#Simulation equivalents:
#Sim2D, ever.far, ever.above.now.below
#(should be possible to use the same exact code on)
#x coordinates of simulations

make.sim.2D <- function(n.moves) {
  
  deltas <- matrix(0, nr=n.moves, nc=2)
  path <- matrix(0, nr=1+n.moves, nc=2)
  
  #Choose x or y as 1 or 2.
  which.dim <- 1 + rbinom(n.moves, size=1, prob=0.5)
  
  #Choose pluses and minuses (for whichever dimension)
  which.move <- rbinom(n.moves, size=1, prob=0.5)
  move <- (-1)^which.move
  
  
  for (i in 1:2) {
    i.inds <- (which.dim==i)
    #Put the correct +/- 1's in the column
    #Others are of course 0
    #Check by rowSumming
    deltas[i.inds,i] <- move[i.inds]
    path[,i] <- c(0, cumsum(deltas[,i]))
  }
  row.check <- rowSums(abs(deltas))
  return(list(deltas=deltas, path=path, row.check = row.check))
}
#Looks good

Sim2D <- function(n.sim, n.moves, fn.path) {
  #n.sim is number of simulations
  #n.moves is number of moves per simulation
  
  #sim.vals <- vector(mode='list', length=n.sim)
  sim.vals <- rep(NA, n.sim)
  for (i.sim in 1:n.sim) {
    #Each path is represented by a column of
    #x's and a column of y's.
    path <- make.sim.2D(n.moves)$path
    sim.vals[i.sim] <- fn.path(path)
  }
  expected <- mean(sim.vals, na.rm=TRUE)
  sigma <- sd(sim.vals, na.rm=TRUE)
  se <- sigma / sqrt(n.sim)
  bad <- mean(is.na(sim.vals))

  return( c(expected + se*c(-1.96, 0, 1.96),
            bad) )
}

is.so.far <- function(r) {
  function(path) {
    #Take last row of path
    xy <- path[ nrow(path) , ]
    distance <- sqrt( sum (xy^2) )
    return(distance >= r)     
  }
}

ever.so.far <- function(r) {
  function(path) {
    distance <- sqrt( rowSums (path^2) )
    return(max(distance) >= r)     
  }
}

mean.time <- function(r) {
  #This one needs n.moves >> r.
  function(path) {
    distance <- sqrt( rowSums (path^2) )
    #Condition on whether you get there
    if (max(distance) >= r) {
      time.till <- min(which(distance >= r))
      return(time.till)
    } else {
      return(NA)      
    }
    #Returns for all cases, function defined
  }
}

ever.east.of <- function(east.bound) {
  function(path) {
    #Just use first column of path.
    #Assume that's horizontal.
    return (max(path[,1]) >= east.bound)
  }
}


ever.east.now.west <- function(east.bound, west.bound) {
  function(path) {
    #Just use first column of path.
    #Assume that's horizontal.
    east.west <- path[,1]
    n <- length(east.west)
    return (max(east.west) >= east.bound & east.west[n] <= west.bound)
  }
}
#Markov equivalents:
#Need: adjust.vector to adjust.matrix
#One routine equivalent to Position and Ever
#where "ever" is either the condition of
#having been >= r^2, or having been east of
#1st st.
#Variables needed in loop:
#Matrices x and y of positions
#Matrix p of probs (starts with 1 at center)
#Matrices p.ever.r2 and p.ever.east

adjust.matrix <- function(x, y, dx=0, dy=0, mult=1) {
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

is.3.10 <- Sim2D(1e6, 10, is.so.far(3))
is.10.60 <- Sim2D(1e6, 60, is.so.far(10))

ever.5.10 <- Sim2D(1e6, 10, ever.so.far(5))
ever.10.60 <- Sim2D(1e6, 60, ever.so.far(10))




