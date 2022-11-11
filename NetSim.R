
# simulate as in .tex example

# returns stream of received 

netSim <- function(nBits,goodToBadProb,badToGoodProb,badProb0) 
{
   trueBits <- sample(0:1,nBits,replace=TRUE)
   rcvdBits <- trueBits  
   state <- 'good'
   for (i in 1:nBits) {  # could use rgeom() and while{} instead
      # check for state change
      if (state == 'good') {
         if (runif(1) < goodToBadProb)  # now bad network
            state <- 'bad'
      } else {  # state == 'bad'
         if (runif(1) < badToGoodProb)  # now bad network
            state <- 'good'
      } 
      if (state == 'bad' && runif(1) < badProb0)
         rcvdBits[i] <- 0
   }
   cbind(trueBits,rcvdBits)
}

