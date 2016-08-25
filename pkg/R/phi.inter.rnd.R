phi.inter.rnd <-
function(iter,grp1,grp2) { 
  grp1.real.ssnos <- unique(grp1$SjNo)
  grp2.real.ssnos <- unique(grp2$SjNo)
  grp1.lp <- length(grp1.real.ssnos)
  grp2.lp <- length(grp2.real.ssnos)
  allsubj <- c(grp1.real.ssnos,grp2.real.ssnos) 
  alldta <- rbind(grp1,grp2)
  nc <- (grp1.lp * grp2.lp)
  phiar <- array(NA,dim=c(nc,5))
  allsubj <- sample(allsubj,length(allsubj),replace=FALSE) 
  grp1.ssnos <- allsubj[1:grp1.lp] 
  grp2.ssnos <- allsubj[(grp1.lp+1):(grp1.lp+grp2.lp)]
  count <- 0
  for ( i in 1:grp1.lp ) {
    es1 <- alldta[alldta$SjNo == grp1.ssnos[i],]
    es1 <- es1[order(es1$ItemNo),]
    for ( j in 1:grp2.lp ) {
      count <- count + 1
      es2 <- alldta[alldta$SjNo == grp2.ssnos[j],]
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
  mcarlo <- mean(phiar[,3],na.rm = TRUE)
  mcarlo
}
