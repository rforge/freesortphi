phi.intra.chance <-
function(iter = 1,numcat,nitems) {
  np <- length(numcat)
  nc <- (np * (np-1))/2
  phiar <- array(NA,dim=nc)
  count <- 0
  for ( i in 1:(np-1) ) {
    for ( j in (i+1):np ) {
      count <- count + 1
      es1 <- sample(1:numcat[i],nitems,replace = TRUE)
      es2 <- sample(1:numcat[j], nitems, replace = TRUE)
      cont <- table(es1,es2)
      k <- min(numcat[i],numcat[j])
      phidem <- nitems * ( k - 1)
      cs <- suppressWarnings(chisq.test(cont,correct=FALSE))
      phiar[count] <- sqrt(cs$statistic / phidem)
    }
  }
  return(mean(phiar,na.rm = TRUE))
}
