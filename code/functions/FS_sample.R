# Function: FS_sample.R
# By: Sabine Loos
# Latest Update: 03.17.2021

# DESCRIPTION: Function used to sample field surveys

# FUNCTIONS: 
## 

# INPUTS:
## 

# OUTPUTS: output contains a list that contains:
## 


# Functions ---------------------------------------------------------------
### break this into multiple sub functions
FS_sample <- function(method = c("random", 
                                 "spcluster",
                                 "biased",
                                 "stratified"),
                      datatosample = sp.pred@data,
                      location = c("haiti", "italy", "nepal", "nz"),
                      areacol = "fsAREA",
                      areaname = list.fsclusterarea[[ind_dmgDat]],
                      nsurvey  = nsurvey,
                      nbldgs = floor(nsurvey*mean(sp.pred$GT_nbldgs, na.rm = T)),
                      seed = mat.seeds[i,ind_pct]){
  set.seed(seed)
  if(method %in% c("random")){
    field_nbldgs <- sum(datatosample[,"GT_nbldgs"])
    while (field_nbldgs > nbldgs) {
      ind_field <- sample(nrow(datatosample), nsurvey, replace = F)
      field_nbldgs <- sum(datatosample[ind_field, "GT_nbldgs"], na.rm = T);
    }
  }
  if("spcluster" %in% method){
    # define field samples (CLUSTERED)
    # 1. reduce to town/district/ward of interest
    fsAREA = areaname # name of town/ward/district
    ind_area <- which(datatosample[,areacol] %in% fsAREA)
    
    # check nsurvey less than fsAREA
    if(nsurvey > length(ind_area)){stop('The number of points to survey is greater than the number of points in the area')}
    
    # 2. Select sample of size nsurvey within the town/district/ward
    ind_sampleinarea <- sample(length(ind_area), nsurvey,  replace = F)
    ind_field <- ind_area[ind_sampleinarea]
  }
  
  if("biased" %in% method){
    require(extraDistr); require(dplyr)
    if(location == "haiti"){
      ### THIS IS GT_67RATE! NOT MEAN CDF ANY MORE
      breaks = c(-0.001, 0.2, 0.4, 0.6, 0.8, 1.001)
      datatosample$GT_OUT <- cut(datatosample$GT_OUT, breaks = breaks)
      levels(datatosample$GT_OUT) <- as.character(1:(length(unique(datatosample$GT_OUT))))
    }else if(location == "nepal"){
      breaks = c(-0.001, 1, 2, 3, 4, 5.001)
      datatosample$GT_OUT <- cut(datatosample$GT_OUT, breaks = breaks)
      levels(datatosample$GT_OUT) <- as.character(1:(length(unique(datatosample$GT_OUT))))
    }else if(location =="nz"){
      breaks = c(-0.001, .01, 0.1, 0.3, 0.6, 1.001) # using atc13 brackets
      datatosample$GT_OUT <- cut(datatosample$GT_OUT, breaks = breaks)
      levels(datatosample$GT_OUT) <- as.character(1:(length(unique(datatosample$GT_OUT))))
    }
    # identify probabilities of each DS using a beta-binomial
    GT_OUT   <- sort(unique(datatosample$GT_OUT))
    x = seq_along(GT_OUT)
    n   <- length(GT_OUT)
    p <- extraDistr::dbbinom(x-1, size = n-1, alpha = 10, beta = 0.8) # negative skew (more collapse), about 55% in top pracket
    # barplot(height = p); sum(p); p

    # assign probabilities to GT_out observations in dataframe
    datatosample$probs <- factor(as.character(datatosample$GT_OUT), levels = as.character(GT_OUT))
    levels(datatosample$probs) <- p
    datatosample$probs <- as.numeric(as.character(datatosample$probs))
    
    # Split probabilities up between number of observations in each class (essentially stratified random sample)
    probs <- factor(as.character(datatosample$probs))
    classcounts <- datatosample %>% group_by(probs) %>% tally()
    levels(probs) <- factor(classcounts$probs/classcounts$n)
    probs <- as.numeric(as.character(probs))
    # sum(probs)
    
    # sample with given probabilities
    ind_field <- sample(nrow(datatosample), nsurvey, prob = probs, replace = F)
    
    # checks 
    # summary(cut(datatosample[ind_field, "GT_OUT"], breaks = breaks)) / sum(summary(cut(datatosample[ind_field, "GT_OUT"], breaks = breaks)) )
    # ggplot(sp.pred@data, aes(y=..count../sum(..count..))) + geom_histogram(aes(GT_OUT, fill = "full dataset"), alpha = 0.5)+
    # geom_histogram(data = sp.pred@data[ind_field,],aes(GT_OUT, fill = "sample"), alpha = 0.5)+
    # labs(title = location)
    # plot(sp.pred)
    # plot(sp.pred[ind_field,], add = T, col = "cyan")
  }
  return(ind_field)
}

## MODIFIED FROM https://github.com/jeffreyevans/spatialEco/blob/master/R/subsample.distance.R
subsample.distance <- function(datatosample, nsurvey, d, d.max = NULL, replacement = FALSE,
                               latlong = FALSE, echo = T) {
  if(latlong == TRUE) {  
    message("geographic projection distances must be in kilometers")
  }
  datatosample$ID <- 1:nrow(datatosample)
  rs <- sample(1:nrow(datatosample),1)
  s <- datatosample[rs,]
  if(replacement == FALSE) { datatosample <- datatosample[-rs,] }
  deval = TRUE	  
  for(i in 2:nsurvey) {	
    nsamp=0
    while(deval == TRUE) { 		  
      rs <- sample(1:nrow(datatosample),1)
      pts.dist = sp::spDists(s, datatosample[rs,], longlat = latlong)
      if(is.null(d.max)) {
        deval <- any(pts.dist < d, na.rm = TRUE)
      } else {
        deval <- any(pts.dist < d, na.rm = TRUE) | any(pts.dist > d.max, na.rm = TRUE)
      } 
      nsamp = nsamp + 1			
      # if(echo) cat("Sample iteration=", nsamp, "\n")  
      if(nsamp == nrow(datatosample)) break			
    }  
    if(echo) {
      cat("\n","Min distance for", i, "=", min(pts.dist, na.rm=TRUE), "\n")
      cat(" Max distance for", i, "=", max(pts.dist, na.rm=TRUE), "\n")
    }
    if(nsamp == nrow(datatosample)) {
      message(paste0("Warning: sampling cannot converge at n=", nsurvey, " returning n=", nrow(s)))
      return(s)  
    }
    deval = TRUE
    s <- rbind(s, datatosample[rs,])
    if(replacement == FALSE) { datatosample <- datatosample[-rs,] } 
  }
  ind_field <- s$ID
  return(ind_field)  
}
