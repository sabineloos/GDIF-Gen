# Function: RK_trend.R
# By: Sabine Loos
# Latest Update: 03.01.2021

# DESCRIPTION: Function used to develop trend model in the framework. 
#              Updated to choose which trend models wanted.
#              using 5-fold cross-validation.

# FUNCTIONS: 
## RK_trend - Develops trend model for given training locations and returns prediction at new locations

# INPUTS:
## locn_shp = Spatial data frame with the locations of the training data (i.e. the field data)
## newdata_shp = Spatial data frame with the locations where you would like to predict the trend at (i.e. the full area of interest)
## stepwise = T/F for whether to perform stepwise selection
## dependent.var_char = character value of the dependent variable for the trend, m (e.g. "GT_OUT")
## independent.var_vec = character vector of the independent variables to considered in the trend, X (e.g. c("DPM_bldg_med", "MMI_mn", "ENG_mnDR", "DEM_mn"))
## id.var_char = character value of the variable that indicates the ID for each location, s (e.g. "GRID_ID")
## return_se = T/F for whether to return the standard errors on the coefficients

# OUTPUTS: output contains a list that contains:
## locn_shp = original locn_shp spatial data frame with added trend variables
## trnd = prediction using OLS
## trnd_resid = residuals from prediction using OLS
## trnd_GLS = prediction using GLS
## trnd_resid_GLS = residuals from prediction using GLS
## newdata_shp = original newdata_shp spatial data frame with added trend variables (same as above)
## vgm = the fitted variogram for GLS model (see autofit package for description of these variables)
## RMSE = dataframe with RMSE from different trend models (used to select best trend model)
## se (optional) = list of standard errors
## ols - standard errors of coefficient estimates from ordinary least squares
## gls - standard errors of coefficient estimates from generalized least squares
## ols.coeff - coefficient values from ordinary least squares estimate
## gls.coeff - coefficient values from generalized least squares estimate

# RK_trend function ----------------------------------------------------
RK_trend <- function(locn_shp = field_sample_shp, 
                        newdata_shp = pred_grid_shp,
                        model = "OLS",
                        stepwise = F,
                        dependent.var_char,
                        independent.var_vec,
                        id.var_char,
                        return_se = F){
  # required packages
  require(caret);
  
  # define the parameters of a 5-fold cross-validation
  n_folds <- 5
  
  # Develop the initial OLS trend -------------------------------------------
  # Create formula with dependent and independent variables
  form <- as.formula(paste(dependent.var_char, paste(independent.var_vec, sep = "", collapse = " + "), sep = " ~ "))
  # set seed for reproducibility
  set.seed(212) 
  
  # recheck for collinearity (here just checking if can't fit modl)
  mod_vif <- lm(form, # formula for linear model with DPM
                locn_shp@data) # data frame of field data at DPM locations
  VIF = car::vif(mod_vif)
  if(any(is.na(coef(mod_vif)))){
    stepwise = T;
    independent.var_vec = names(coef(mod_vif))[which(!is.na(coef(mod_vif)))[-1]]
    form <- as.formula(paste(dependent.var_char, paste(independent.var_vec, sep = "", collapse = " + "), sep = " ~ "))
    }
  
  # develop linear OLS models using stepwise selection (for multicollinearity) or all independent variables
  if(model == "OLS"){
    train.control <- caret::trainControl(method = "cv", number = 5, allowParallel = F)
      if(stepwise == T){
      # Here we use mixed stepwise selection for OLS regression
      # do mixed stepwise selection using 5-fold cross-validation to choose the best model with best subset of predictors
      # Train the model
      lm.OLS <- caret::train(form, data = locn_shp@data,
                             method = "leapSeq", # can also be leapForward (for forward selection) and leapBackward (for backward selection) 
                             tuneGrid = data.frame(nvmax = 1:length(independent.var_vec)), #  the number of variable in the model
                             trControl = train.control)
      beta_OLS <- coef(lm.OLS$finalModel, id = lm.OLS$bestTune$nvmax)# save the coefficients
      lm_terms <- names(beta_OLS)[-1] # terms kept in the OLS model
      newdata_shp$trnd <- as.vector(as.matrix(cbind(int = rep(1, nrow(newdata_shp)),newdata_shp@data[,lm_terms])) %*% beta_OLS) # predicting at new locations
      RMSE_OLS <- mean(lm.OLS$resample$RMSE) # what is the average RMSE of all 5 folds? use for model selection
    }else{# otherwise no mixed stepwise selection
      lm.OLS <- caret::train(form, data = locn_shp@data,
                             method = "lm", # no stepwise selection
                             trControl = train.control)
      beta_OLS <- coef(lm.OLS$finalModel) # save the coefficients
      lm_terms <- names(beta_OLS)[-1] # terms in the OLS model (this should be the same as dependent.vars)
      newdata_shp$trnd <- as.vector(as.matrix(cbind(int = rep(1, nrow(newdata_shp)),newdata_shp@data[,lm_terms])) %*% beta_OLS) # predicting at new locations
      RMSE_OLS <- mean(lm.OLS$resample$RMSE) # what is the average RMSE of all 5 folds? use for model selection
    }
    # Estimation variance calculation for OLS
    n = nrow(locn_shp) # n = number of locations in training data
    n0 = nrow(newdata_shp) # number of new locations
    q <- as.matrix(cbind(int = rep(1, n),locn_shp@data[,lm_terms])) # predictor matrix at training locations
    q0 = t(cbind(int = rep(1, n0), newdata_shp@data[,lm_terms])) # predictor matrix at new locations
    newdata_shp$trnd_var <- colSums(t(t(q0) %*% solve(t(q) %*% q)) * q0) # OLS trend variance
    
    coeff = as.data.frame(t(as.matrix(beta_OLS)))
    # standard error
    if(return_se == T){
      # calculate standard errors of OLS
      X <- as.matrix(cbind(int = rep(1, nrow(locn_shp@data)),locn_shp@data[,lm_terms]))
      fitted.values <- X %*% beta_OLS
      res <- locn_shp@data[,dependent.var_char] - fitted.values
      df <- length(is.na(locn_shp@data[,dependent.var_char])) - length(lm_terms) - 1 # n - p - 1 with intercept
      sig <- sum(res^2)/df
      se.OLS <- sqrt(diag(solve(t(X) %*% X)) * sig)
    }
  }else if(model == "tree"){
    require(rpart); 
    # training
    tree.mod <- rpart(formula = form, data = locn_shp@data, method="anova", 
                      control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, 
                                              usesurrogate = 2, # 2 means use surrogate split
                                              xval = n_folds))
    coeff = tree.mod$variable.importance
    # caret::postResample(predict(tree.mod, locn_shp@data), locn_shp$GT_meanCDF) 
    # predict at new data
    newdata_shp$trnd <- predict(tree.mod, newdata_shp)
  }
  
  # Calculate residuals
  locn_shp <- sp::merge(locn_shp, newdata_shp@data[,c(id.var_char, "trnd")], by = id.var_char, all.x = T) # merge prediction at new with training locations
  locn_shp$trnd_resid <- locn_shp@data[,dependent.var_char] - locn_shp$trnd # calculate the residuals (difference between truth and prediction)
  
  # Saving outputs -------------------------------------------
  # dataframe of RMSE's
  # RMSE = data.frame(model = c("trnd"), RMSE = c(RMSE_OLS))
  # if you want to return the standard errors
  if(return_se == T){
    # return gamma, C, and shapes
    return(list(locn_shp = locn_shp, newdata_shp = newdata_shp, 
                coeff = coeff, VIF = VIF, mod = lm.OLS, se = se.OLS))
  }else{
    # return gamma, C, and shapes
    return(list(locn_shp = locn_shp, newdata_shp = newdata_shp, 
                coeff = coeff, VIF = VIF, mod = lm.OLS))
  }
}

## modifying caret's initial expand grid (from getModelInfo("rpart")[[1]]$grid)

treegrid <- function(x, y, len = nrow(initialFit), search = "grid"){
  dat <- if(is.data.frame(x)) x else as.data.frame(x)
  dat$.outcome <- y
  initialFit <- rpart::rpart(.outcome ~ .,
                             data = dat,
                             control = rpart::rpart.control(cp = 0))$cptable
  initialFit <- initialFit[order(-initialFit[,"CP"]), , drop = FALSE]
    if(nrow(initialFit) < len) {
      tuneSeq <- data.frame(cp = seq(min(initialFit[, "CP"]),
                                     max(initialFit[, "CP"]),
                                     length = len))
    } else tuneSeq <-  data.frame(cp = initialFit[1:len,"CP"])
  
  tuneSeq
}
