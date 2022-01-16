# packages to be installed if not already
install.packages("car", repos='http://cran.us.r-project.org'); 
install.packages("caret", repos='http://cran.us.r-project.org')
install.packages("gstat", repos='http://cran.us.r-project.org')
install.packages("sp", repos='http://cran.us.r-project.org')
install.packages("raster", repos='http://cran.us.r-project.org')
install.packages("ggplot2", repos='http://cran.us.r-project.org')
install.packages("gridExtra", repos='http://cran.us.r-project.org')
install.packages("dplyr", repos='http://cran.us.r-project.org')
install.packages("rgeos", repos='http://cran.us.r-project.org')
install.packages("rgdal", repos='http://cran.us.r-project.org')
# for RK_trend.R, this is optional, only if want to try a regression tree
install.packages("rpart", repos='http://cran.us.r-project.org')
# for OK_Parallel.R, this is optional if not running in parallel
install.packages("foreach", repos='http://cran.us.r-project.org')
install.packages("future", repos='http://cran.us.r-project.org')
install.packages("doFuture", repos='http://cran.us.r-project.org')
# for PlottingFunctions.R, some of these are optional
install.packages("rasterVis", repos='http://cran.us.r-project.org')
install.packages("GGally", repos='http://cran.us.r-project.org')
install.packages("ggsn", repos = 'http://cran.us.r-project.org')