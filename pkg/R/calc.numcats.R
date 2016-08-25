calc.numcats <-
function(edta) {
  ssnos <- unique(edta$SjNo)
  np <- length(ssnos)
  numcat <- array(NA,np)
  for (i in 1:np) {
    es1 <- edta[edta$SjNo == ssnos[i],]
    numcat[i] <- length(unique(es1$CategoryNo))
  }
  numcat
}
