#' Run lm() ROI stats in a linear model, per your specifications
#' 
#' @param df A data frame in long format, organized as QuANTs-style output: dependent variable (freq. thicknesses, volumes, or cbf values, etc) in a column called "value"
#'    a column called "label" with the label numbers. Each row is one measurement for one person. Can have as many columns as you want (ie, ignores everything not specified")
#'    Also requires columns named as specified  used as varOfInterest, covarOfInterest (if used), and otherCovarString (if used)
#' @param regressionString An "lm-compatible" string of what you want your regression to be, using the column names of your df 
#'    eg, "value~PatientGroup+AgeAtMRI+PatientGroup:AgeAtMRI"
#' @param lut A look up table with label numbers in the first column and label names in the second column. Can have extra rows or columns, they will be ignored 
#' @return A list object with 3 elements 
#'     1) a df containing significant labels from the lm()
#'     2) summay(lm()) output from each label in your input df 
#'     3) a plot for each region in your df 1) (ie, one plot for each significant label)
#' @import broom
#' @export



runLms <- function (df, regressionString, lut)  {
  # check for look up table / create dummy one 
  if (missing(lut)) {
    print("No look up table, wingin' it")
    lut <- rbind(unique(df$label), paste("label",unique(df$label), sep=""))
    lut <- t(lut)
    lut[,1] <- as.double(lut[,1])
  }
  
  # get rid of extra factor levels if i'm lazily filtering
  df <- droplevels(df)
  # set up output stats matrix 
  numLabels <- length(unique(df$label))
  statslist <- list()
  
  # if using more labels than you think you are, stop it 
  if (dim(lut)[1] < numLabels ) {
    stop("more labels in df than in lut, stopping...")
  }
  
  # string vs formula vs other object parsing in R is garbage 
  # in any case, create a new object that's the formula to put into lm()
  #                        (cbf)    (what we specify for input into our model)
  # bunch of stuff for getting strings into happy lm()-fitting format 
  if (missing(regressionString)) {
    stop("need regressionString to run lm")
  }
  print(regressionString)
  frm <- as.formula(regressionString)
  #statsdf <- 
  # run same stats for each label
  for (i in 1:numLabels) {
    # get label for this regression
    tmpLabel <- unique(df$label)[i]
    # set up to run linear regression
    #                                subset our whole input matrix for just this label, and run the lm on that
    labeldf <- df[df$label==tmpLabel,]
    tmplm <- lm(frm, data=labeldf)
    labeldf$resid <- tmplm$residuals
    tmpstats <- tidy(tmplm)
    tmpstats <- cbind(lut[lut[,1] == tmpLabel, 2], tmpstats)
    tmpstats <- cbind(tmpLabel, tmpstats)
    colnames(tmpstats)[1:2] <- c("label", "Label.Names")
    if (i == 1){
      statsdf <- tmpstats
      dfresid <- labeldf
    } else {
      statsdf <- rbind(statsdf, tmpstats)
      dfresid <- rbind(dfresid, labeldf)
    }
  }
  statsAndDf <- list()
  statsAndDf$statstable <- statsdf
  statsAndDf$outdf <- dfresid
  return(statsAndDf)
}