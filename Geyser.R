
geyserSim <- function(nTimePers,AtoB,BtoA) 
{
   x <- vector(length=nTimePers)
   state <- 'A'
   for (i in 1:nTimePers) {  # could use rgeom() and while{} instead
      # check for state change
      if (state == 'A') {
         if (runif(1) < AtoB)  # now bad network
            state <- 'B'
      } else {  # state == 'B'
         if (runif(1) < BtoA)  # now bad network
            state <- 'A'
      } 
      if (state == 'B') x[i] <- rnorm(1)
      else x[i] <- rnorm(1) + 2
   }
   x
}

