# Function: RK_errormet.R
# By: Sabine Loos
# Latest Update: 08.23.2021

# DESCRIPTION: Function used to quantify various errormetrics between two vectors, used to quantify errors of the framework and engineering forecast

# FUNCTIONS: 
## RK_errormet - quantifies error metrics of the prediction and uncertainty estimate

# INPUTS:
## newdata = data frame with the predictions
## dependent.var_char = character value of the true dependent variable(e.g. "GT_OUT")
## pred.var_char = character value of the predicted variables to be compared to the true value (e.g. "RK_pred_fin")
## var.var_char = character value of the predicted varirance (e.g. "var_fin")
## ind_validation = vector of the index of newdata used for validation
## error = T/F calculate error (e.g. predicted - observed)
## error_rel = T/F calculate relative error (e.g. (predicted-observed)/observed)
## error_sq = T/F calcuate the squared error (e.g. (predicted - observed)^2)
## error_abs = T/F calculate the absolute error (e.g. |predicted - observed|)
## SDSR = T/F calculate the standard deviation of the standardized residuals
## accplot = T/F calculate the accuracy plot and goodness metric G of the uncertainty
## R_sq = T/F calculate the explained variance (aka the R^2)

# OUTPUTS: output contains a list that contains:
## errormet =  dataframe with  chosen errormetrics as columns

# RK_errormet -------------------------------------------------------------
RK_errormet <- function(newdata, 
                        dependent.var_char = "GT_OUT",
                        pred.var_char = "RK_pred_fin",
                        var.var_char = "var_fin",
                        n_pred = NULL,
                        ind_validation,
                        error = T,
                        error_sq = T,
                        error_abs = T,
                        SDSR = T,
                        accplot = T, 
                        R_sq = T){
  if(class(newdata) == "SpatialPointsDataFrame"){newdata = newdata@data}
  # initialize dataframe to hole error metrics
  errormet <- data.frame(del = NA)
  # calculate the error: predicted - observed
  ERROR <- rowSums(cbind(newdata[,pred.var_char], - newdata[, dependent.var_char]), na.rm = F)
  
  # calculate error metrics
  if(error == T){ # error 
    newdata$ERROR <- rowSums(cbind(newdata[,pred.var_char], - newdata[, dependent.var_char]), na.rm = F) # add column of error
    errormet$ME <- mean(newdata$ERROR[ind_validation], na.rm = T) # mean error
    errormet$VE <- var(newdata$ERROR[ind_validation], na.rm = T) # variance of the error
    
  }
  if(error_sq == T){
    newdata$ERROR_sq <- ERROR^2 # add column of squared error
    errormet$MSE  <- mean(newdata$ERROR_sq[ind_validation], na.rm = T) # mean squared error
  }
  if(error_abs == T){
    newdata$ERROR_abs <- abs(ERROR) # add column of absolute error
    errorsum <- rowSums(cbind(newdata[,pred.var_char], newdata[, dependent.var_char]), na.rm = F)
    errormet$MAE <- mean(newdata$ERROR_abs[ind_validation], na.rm = T) # mean absolute error
    errormet$SMAPE <- sum(newdata$ERROR_abs[ind_validation], na.rm = T)/sum(errorsum[ind_validation], na.rm = T)  # symmetric mean abs. percentage error https://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error 
  }

  if(SDSR == T){
    errormet$SDSR <- sd(ERROR[ind_validation]/sqrt(newdata[ind_validation, var.var_char]), na.rm = T) # # add column of standard deviation of standardized residual
  }
  if(accplot == T){ # accuracy plot
    # The accuracy plot looks at the accuracy of the uncertainty
    # more information can be found in:
    # Deutsch, C., 1997. Direct assessment of local accuracy and precision. In Baafi, E. and Schofield, N.
    # (eds.), GeostatisticsWollongong ’96, 1 edn., pp. 115–125. Kluwer Academic Publishers,Wollongong, Australia.
    p_RK = seq(0,100, by = 0.5)
    sd <- qnorm((0.5 + p_RK/200)); sd[length(p_RK)] <- qnorm(0.999999)
    psi_TRUE <- sapply(X = sd, function(x, mid = newdata[,pred.var_char], 
                                               var_fin = newdata[,var.var_char],
                                               GT_OUT = newdata[, dependent.var_char]){
                                              sd = x
                                              plus_sd <- mid+sd*sqrt(var_fin)
                                              minus_sd <- mid-sd*sqrt(var_fin)
                                              within_sd <- ifelse(GT_OUT < plus_sd & GT_OUT > minus_sd, 1, 0)
                                              return(100*(sum(within_sd[ind_validation], na.rm = T)/nrow(newdata)))})
    # calculate goodness metric, G
    errormet$G = 1 - sum(mapply(function(psi, p){
        diff <-  psi - p
        a <- ifelse(diff >= 0, 1, -2)
        sig <- a * diff
        return(sig)}, psi = psi_TRUE/100, p = p_RK/100)*(1/length(p_RK)))
  }
  if(R_sq == T){
    pred <- newdata[,pred.var_char]
    obs <- newdata[,dependent.var_char]
    errormet$R_sq = cor(obs[ind_validation], pred[ind_validation], use = "complete.obs")^2
    n = nrow(newdata[-ind_validation,])
    errormet$R_sqadj = 1-(1-errormet$R_sq)*(n-1)/(n-n_pred-1)
    
  }
  # return outputs
  return(errormet[-which(names(errormet) %in% "del")])
}
