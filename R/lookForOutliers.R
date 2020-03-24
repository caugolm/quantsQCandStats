#' Create a histogram and note number of outliers for a quick QC report 
#' 
#' @param df A data frame in long format, organized as QuANTs-style output: variable of interest in a column called "value", a column called "measure" for labeling
#' @param sigmas The number of standard deviations away from the mean you want to consider "normal"
#' @param groupVar Column name you want to group outliers by (common uses: label, ID, and/or MRIDate)  
#' @return A list object with object "histogram" which displays a histogram of all included data, with the outliers highlighted
#'     Also returns a dataframe with the number of outliers per value specified in groupVar
#' @import ggplot2
#' @export

outlierDetection <- function(df, sigmas, groupVar){
    # returns a list object containing a figure and 
    # an object containing subject id as names with the number of values they have outside of input sigmas from the mean 
    # get mean and sd 
    meanall <- mean(df$value)
    sdall <- sd(df$value)
    # get range values
    ddrange <- c(meanall-sigmas*sdall, meanall+sigmas*sdall)
    # append a temporary column with whether value is inside or outside of specified number of sigmas 
    df$sigmaTest[df$value < ddrange[1] | df$value > ddrange[2]] <- "outliers"
    df$sigmaTest[df$value > ddrange[1] & df$value < ddrange[2]] <- "inliers"
    # ggplot histogram with your specified sigmas color coded 
    plotout <- ggplot(df, aes(x = value, fill=sigmaTest)) + geom_histogram() + xlab(label = df$measure[1]) + 
      scale_fill_discrete(name=c(paste("Mean",round(meanall,3),"\nSD ", round(sdall,3), "\n",sigmas,"sigmas")))
    # obnoxious table with number of times each id had regions below and above min [1] and max "range" values 
    tabletest <- table(df[df$value < ddrange[1] | df$value > ddrange[2], names(df) %in% groupVar])
    tableouts <- tabletest[tabletest > 0]
    tableouts <- sort(tableouts, decreasing = T)
    # put it all in a list and name it politely
    outdf <- list()
    outdf[[1]] <- plotout
    outdf[[2]] <- tableouts
    names(outdf) <- c("histogram","outlierCounts")
    return(outdf)
  }
  