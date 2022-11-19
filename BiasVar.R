
# experiments to show bias-variance tradeoff; key is showing that var
# increases at all steps

biasVar <- function(data,yName,deg) 
{
   require(qeML)
   yCol <- which(names(data) == yName)
   cols <- c(setdiff(1:ncol(data),yCol),yCol)
   data <- data[,cols]
   set.seed(9999)
   pfout <- polyFit(data,deg)
   vcv <- vcov(pfout$fit)
   x1 <- data[1,-ncol(data)]
   x1 <- as.matrix(x1)
   x1 <- getPoly(x1,deg)$xdata
   x1 <- matrix(x1[1,],nrow=1)
   x1 <- as.numeric(x1)
   x1 <- c(1,x1)
   x1 <- matrix(x1,nrow=1)
   x1 %*% vcv %*% t(x1)

}
