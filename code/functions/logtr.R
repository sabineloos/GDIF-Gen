# taking the log of each variable, to create a normal-ish distribution
logtr <- function(x){
  # handles NAs
  x[x!=0 & !is.na(x)] <- log10(x[x!=0 & !is.na(x)]);
  # if 0, add small amount
  x[x==0 & !is.na(x)] <- log10((x[x==0 & !is.na(x)]+0.00000001))
  return(x)
}

# same as above, but then standardize to have mean of zero and s.dev of 1
logtr_std <- function(x){
  # handles NAs
  x[x!=0 & !is.na(x)] <- log10(x[x!=0 & !is.na(x)]);
  # if 0, add small amount
  x[x==0 & !is.na(x)] <- log10((x[x==0 & !is.na(x)]+0.00000001))
  
  # standardize to have mean 0 and unit norm
  x <- std(x)
  return(x)
}

# log transform, then standardize, then min-max scale
logtr_scale_std <- function(x){
  # handles NAs
  x[x!=0 & !is.na(x)] <- log10(x[x!=0 & !is.na(x)]);
  # if 0, add small amount
  x[x==0 & !is.na(x)] <- log10((x[x==0 & !is.na(x)]+0.00000001))
  
  # scale to be 0-1
  x <- scale_01(x)
  # standardize to have mean 0 and unit norm
  x <- std(x)
  return(x)
}

## for scaling and standardizing
scale_std <- function(x){
  # scale to be 0-1
  x <- scale_01(x)
  # standardize to have mean 0 and unit norm
  x <- std(x)
  return(x)
}

## standardize - have mean of 0 and s.dev or 1
std <- function(x){
  # standardize to have mean 0 and s.dev of 1
  x <- (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  return(x)
}

## for scaling
scale_01 <-function(x){
  dif = diff(range(x, na.rm = T))
  x = (x-min(x, na.rm = T))/diff(range(x, na.rm = T))
  return(x)
}
