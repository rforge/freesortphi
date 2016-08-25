phi.inter <-
function(grp1,grp2) {
  grp1.ssnos <- unique(grp1$SjNo)
  grp2.ssnos <- unique(grp2$SjNo)
  grp1.lp <- length(grp1.ssnos)
  grp2.lp <- length(grp2.ssnos)
  nc <- (grp1.lp * grp2.lp)
  phiar <- array(NA,dim=c(nc,5))
  count <- 0
  for ( i in 1:grp1.lp ) {
    es1 <- grp1[grp1$SjNo == grp1.ssnos[i],]
    es1 <- es1[order(es1$ItemNo),]
    for ( j in 1:grp2.lp ) {
      count <- count + 1
      es2 <- grp2[grp2$SjNo == grp2.ssnos[j],]
      es2 <- es2[order(es2$ItemNo),]
      cont <- table(es1$CategoryNo,es2$CategoryNo)
      r <- nrow(cont)
      c <- ncol(cont)
      n <- sum(cont)
      k <- min(r,c)
      notk <- max(r,c)
      phidem <- n * ( k - 1)
      phiar[count,1] <- grp1.ssnos[i]
      phiar[count,2] <- grp2.ssnos[j]
      cs <- suppressWarnings(chisq.test(cont,correct=FALSE))
      phiar[count,3] <- sqrt(cs$statistic / phidem)
      phiar[count,4] <- k
      phiar[count,5] <- notk
    }
  }
  retval <- mean(phiar[,3],na.rm = TRUE)
  retval
}
