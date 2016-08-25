sig.con <-
function(x,val) {
    dum <- quantile(x,c(.99,.95,.90,.1,.05, .01))
  ret <- "n.s."
  if (val < dum["10%"]) ret <- "< .1"
  if (val < dum["5%"]) ret <- "< .05"
  if (val < dum["1%"]) ret <- "< .01"
  if (val > dum["90%"]) ret <- "< .1"
  if (val > dum["95%"]) ret <- "< .05"
  if (val > dum["99%"]) ret <- "< .01"
  ret
}
