
# experiments to show bias-variance tradeoff; key is showing that var
# increases at all steps

biasVar <- function(data,yName,deg) 
{
   require(qeML)
   yCol <- which(names(data) == yName)
   cols <- c(setdiff(1:ncol(data),yCol),yCol)
   data <- data[,cols]
   tstRows <- sample(1:nrow(data),1000)
   trn <- data[-tstRows,]
   tst <- data[tstRows,]
   pfout <- polyFit(trn,deg)
   vcv <- vcov(pfout$fit)
   x1 <- trn[1,-ncol(data)]
   x1 <- as.matrix(x1)
   x1 <- getPoly(x1,deg)$xdata
   x1 <- matrix(x1[1,],nrow=1)
   x1 <- as.numeric(x1)
   x1 <- c(1,x1)
   x1 <- matrix(x1,nrow=1)
   x1var <- x1 %*% vcv %*% t(x1)
   preds <- predict(pfout,tst[,-ncol(tst)])
   testAcc <- mean(abs(tst[,ncol(tst)] - preds))
   c(x1var,testAcc)
}
