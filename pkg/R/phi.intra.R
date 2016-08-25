phi.intra <-
function(edta) {
  ssnos <- unique(edta$SjNo)
  fp <- 1
  lp <- length(ssnos)
  np <- lp - fp + 1
  nc <- (np * (np-1))/2
  phiar <- array(NA,dim=c(nc,5))
  count <- 0
  for ( i in fp:(lp-1) ) {
    es1 <- edta[edta$SjNo == ssnos[i],]
    es1 <- es1[order(es1$ItemNo),]
    for ( j in (i+1):lp ) {
      count <- count + 1
      es2 <- edta[edta$SjNo == ssnos[j],]
      es2 <- es2[order(es2$ItemNo),]
      cont <- table(es1$CategoryNo,es2$CategoryNo)
      r <- nrow(cont)
      c <- ncol(cont)
      n <- sum(cont)
      k <- min(r,c)
      notk <- max(r,c)
      phidem <- n * ( k - 1)
      phiar[count,1] <- ssnos[i]
      phiar[count,2] <- ssnos[j]
      cs <- suppressWarnings(chisq.test(cont,correct = FALSE))
      phiar[count,3] <- sqrt(cs$statistic / phidem)
      phiar[count,4] <- k
      phiar[count,5] <- notk
    }
  }
  retval <- mean(phiar[,3],na.rm = TRUE)
  retval
}
