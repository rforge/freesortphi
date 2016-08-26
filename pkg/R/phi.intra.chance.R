phi.intra.chance <-
function(iter = 1,numcat,nitems,missing=NULL) {
  np <- length(numcat)
  nc <- (np * (np-1))/2
  phiar <- array(NA,dim=nc)
  count <- 0
  for ( i in 1:(np-1) ) {
    for ( j in (i+1):np ) {
        count <- count + 1
        if( is.null(missing) ) {
           # print('None missing')
            es1 <- sample(1:numcat[i],nitems,replace = TRUE)
            es2 <- sample(1:numcat[j],nitems, replace = TRUE)
        } else {
            # print('Some missing')
            es1 <- sample(1:numcat[i],nitems-missing[i],replace = TRUE)
            es2 <- sample(1:numcat[j],nitems-missing[j],replace = TRUE)
            es1 <- c(es1,rep(NA,missing[i]))
            es1 <- sample(es1)
            es2 <- c(es2,rep(NA,missing[j]))
            es2 <- sample(es2)
        }
        cont <- table(es1,es2)
        k <- min(numcat[i],numcat[j])
        phidem <- nitems * ( k - 1)
        cs <- suppressWarnings(chisq.test(cont,correct=FALSE))
        phiar[count] <- sqrt(cs$statistic / phidem)
    }
  }
  return(mean(phiar,na.rm = TRUE))
}
