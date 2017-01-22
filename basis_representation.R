toBaseArb <- function(x, base){
  bin_len <- log(x, base=base) - log(x, base=base)%%1 + 1
  bin_rep <- rep(0, bin_len)
  
  if(log(x, base=base) < 1){
    bin_rep[1] <- x
    return(bin_rep)
  }
  
  i <- TRUE
  
  while(i) {
    n <- log(x, base=base) - log(x, base=base)%%1
    bin_rep[bin_len - n] <- bin_rep[bin_len - n] + 1
    if(log(x, base=base)%%1 == 0){
      i <- FALSE
    } else {
      x <- x-base^n
    }
  }

  return(bin_rep)
}