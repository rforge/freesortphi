calc.missing <-
function(edta) {
  ssnos <- unique(edta$SjNo)
  np <- length(ssnos)
  num.miss <- array(NA,np)
  for (i in 1:np) {
    es1 <- edta[edta$SjNo == ssnos[i],]
    num.miss[i] <- length(es1$CategoryNo) - length(es1$CategoryNo[!is.na(es1$CategoryNo)])
  }
  num.miss
}
