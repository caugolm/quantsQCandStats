#' Run a bunch of ROI stats in a linear model, per your specifications
#' 
#' @param df A data frame in long format, organized as QuANTs-style output: dependent variable (freq. thicknesses, volumes, or cbf values, etc) in a column called "value"
#'    a column called "label" with the label numbers. Each row is one measurement for one person. Can have as many columns as you want (ie, ignores everything not specified")
#'    Also requires columns named as specified  used as varOfInterest, covarOfInterest (if used), and otherCovarString (if used)
#' @param varOfInterest A column name from df that you want to run stats on. Often either a column containing group status info or a score for a regression
#' @param covarOfInterest A column name you want to use as a covariate in your linear model. Will also be used for plotting.
#' @param otherCovarString A string you want to use for covariates of no interest. Won't be used for plots. For multiple values, use lm()-style formula "Covar1+Covar2+..." 
#' @param lut A look up table with label numbers in the first column and label names in the second column. Can have extra rows or columns, they will be ignored 
#' @param sigValue A value used for thresholding your statistics. If you want to show all data use 1.
#' @param multipleCorrectionMethod Goes into p.adjust() to account for multiple comparisons. Will do this accounting for all labels in your input df. Default is none, common other choices are "bonferroni" and "fdr"
#' @param outFileRoot Optional file root for output. If specified, will save 1) .txt file with all significant results and 2) plots for up to 5 labels (with 5 smallest p-values)
#' @return A list object with 3 elements 
#'     1) a df containing significant labels from the lm()
#'     2) summay(lm()) output from each label in your input df 
#'     3) a plot for each region in your df 1) (ie, one plot for each significant label)
#' @export
#' 


getStats <- function (df, varOfInterest, covarOfInterest, otherCovarString, lut, sigValue, multipleCorrectionMethod, outFileRoot)  {
  # we might want multiple corrections, but we might not. 
  if (missing(multipleCorrectionMethod)) {
    multipleCorrectionMethod <- "none"
  }
  
  # check for look up table / create dummy one 
  if (missing(lut)) {
    print("No look up table, wingin' it")
    lut <- rbind(unique(df$label), paste("label",unique(df$label), sep=""))
  }
  
  # get rid of extra factor levels if i'm lazily filtering
  df <- droplevels(df)
  # set up output stats matrix 
  numLabels <- length(unique(df$label))
  statsmat <- as.data.frame(matrix(NA,nrow=numLabels, ncol = 6))
  statslist <- list()
  
  # if using more labels than you think you are, stop it 
  if (dim(lut)[1] < numLabels ) {
    stop("more labels in df than in lut, stopping...")
  }
  
  # string vs formula vs other object parsing in R is garbage 
  # in any case, create a new object that's the formula to put into lm()
  #                        (cbf)    (what we specify for input into our model)
  # bunch of stuff for getting strings into happy lm()-fitting format 
  if (missing(varOfInterest)) {
    stop("need varOfInterest assigned to run lm")
  }
  regressionString <- paste("value", '~', varOfInterest, sep=" ")
  if (!missing(covarOfInterest) & !is.na(covarOfInterest)) {
    regressionString <- paste(regressionString, "+" , covarOfInterest, sep = " ")
  }
  if (!missing(otherCovarString) & !is.na(otherCovarString)) {
    regressionString <- paste(regressionString, "+" , otherCovarString, sep = " ")
  }
  print(regressionString)
  frm <- as.formula(regressionString)
  
  # run same stats for each label
  for (i in 1:numLabels) {
    # get label for this regression
    tmpLabel <- unique(df$label)[i]
    # put it in your output matrix
    statsmat[i,1]<- tmpLabel
    # give it index (helpful if returning the list with all stats output) 
    statsmat[i,5] <- i
    # give it a name 
    statsmat[i,6] <- lut[lut[,1] == tmpLabel, 2]
    # set up to run linear regression
    #                                subset our whole input matrix for just this label, and run the lm on that
    statslist[[i]] <- summary(lm(frm, data=df[df$label==tmpLabel,]))
    # organize stats output:
    # assume first input is most important. Put the t-stat for it here.
    statsmat[i,2]<- statslist[[i]]$coefficients[2,3]
    # and get the corresponding p-value 
    statsmat[i,3]<- statslist[[i]]$coefficients[2,4]
  }
  # we may want to correct for multiple comparisons. If specified in function input, do that now:
  #           statsmat[,3] is the p value column for all labels 
  statsmat[,4] <- p.adjust(statsmat[,3],multipleCorrectionMethod)
  # label columns appropriately
  colnames(statsmat) <- c("label","tstat","pvalue","p_adj","index","LabelName")
  # threshold your ROIs by whatever threshold specified 
  sigResults <- statsmat[statsmat[,4] < sigValue, ]
  sigResults <- as.data.frame(sigResults)
  
  # order stats from smallest p-value to largest
  sigResults <- sigResults[with(sigResults,order(p_adj)),]
  
  # if there's an output file root, save the significant results table
  if (!missing(outFileRoot)) {
    # write output in format for the lausanneTableToFigure.sh script
    outRoot <- paste(outFileRoot, "_", multipleCorrectionMethod, sigValue, sep="")
    write.table(sigResults,paste(outRoot,".txt", sep=""), quote = F, row.names = F, col.names=F)
  }
  
  # create a flexible (list) object to store all of the significant plots
  sigPlots <- list()
  # but only actually make the plots if there is something to plot
  if (nrow(sigResults) > 0) {  
    # iterate over each significant roi
    for ( s in 1:dim(sigResults)[1]) {
      # hold label for ease later
      loi <- sigResults$label[s]
      # stupid ggplot variable vs column name handling
      ggvarOfInterest <- ensym(varOfInterest)
      # if there's no covariate, just plot the main effect
      if (missing(covarOfInterest) | is.na(covarOfInterest)){ 
        # if our variable of interest is an integer or double or numeric, make a scatterplot, 
        # but if it's a factor or character then make a boxplot 
        if ( (class(df[,varOfInterest]) == "integer" | class(df[,varOfInterest]) == "double" | class(df[,varOfInterest]) == "numeric" ) ) {
          sigPlots[[s]] <- ggplot(df[df$label == loi ,], aes(x= !!ggvarOfInterest, y=value)) + geom_point() + geom_smooth(method = "lm") + ggtitle(paste(outFileRoot, lut[lut[,1] == loi,2]))
        } else {
          sigPlots[[s]] <- ggplot(df[df$label==loi ,], aes(x= !!ggvarOfInterest, y=value, fill=!!ggvarOfInterest)) + geom_boxplot() + geom_jitter(height = 0, width = .3) + scale_fill_manual(values=mutPal) + ggtitle(paste(outFileRoot, lut[lut[,1] == loi,2]))
        }
        # if we have our main effect and a covariate, we have a bunch of options, but in essense if it's a factor make it a color and plot the x-axis with the numeric one, but if there's two numberic ones color by the covariate and plot on the main variable
      } else {
        ggcovarOfInterest <- ensym(covarOfInterest)
        if ( (class(df[,varOfInterest]) == "factor" | class(df[,varOfInterest]) == "character") & (class(df[,covarOfInterest]) == "integer" | class(df[,covarOfInterest]) == "double" | class(df[,varOfInterest]) == "numeric") ) {
          sigPlots[[s]] <- ggplot(df[df$label==loi ,], aes(x= !!ggcovarOfInterest, y=value, color= !!ggvarOfInterest)) + geom_point() + geom_smooth(method = "lm") + scale_color_manual(values=mutPal) + ggtitle(paste(outFileRoot, lut[lut[,1] == loi,2]))
        } else if ( (class(df[,varOfInterest]) == "factor" | class(df[,varOfInterest]) == "character") & (class(df[,covarOfInterest]) == "factor" | class(df[,covarOfInterest]) == "character") ) {
          sigPlots[[s]] <- ggplot(df[df$label==loi ,], aes(x= !!ggvarOfInterest, y=value, fill= !!ggcovarOfInterest)) + geom_boxplot() + geom_jitter(height = 0, width = .3) + ggtitle(paste(outFileRoot, lut[lut[,1] == loi,2]))
        } else if ( (class(df[,varOfInterest]) == "integer" | class(df[,varOfInterest]) == "double" | class(df[,varOfInterest]) == "numeric") & (class(df[,covarOfInterest]) == "factor" | class(df[,covarOfInterest]) == "character") ) {
          sigPlots[[s]] <- ggplot(df[df$label==loi ,], aes(x= !!ggvarOfInterest, y=value, color= !!ggcovarOfInterest)) + geom_point() + geom_smooth(method = "lm") + ggtitle(paste(outFileRoot, lut[lut[,1] == loi,2]))
        } else {
          sigPlots[[s]] <- ggplot(df[df$label==loi ,], aes(x= !!ggvarOfInterest, y=value, color= !!ggcovarOfInterest)) + geom_point() + geom_smooth(method = "lm") + ggtitle(paste(outFileRoot, lut[lut[,1] == loi,2]))
        }
      }
      numPlots <- dim(sigResults)[2]
      if (!missing(outFileRoot)) {
        # write output in format for the lausanneTableToFigure.sh script
        if (numPlots > 6) {
          numPlots <- 5
        }
        gridplots <- grid.arrange(sigPlots[1:numPlots])
        ggsave(paste(outRoot, "sig", numPlots, ".png", sep="") ,gridplots )
      }
    }
  }

  outlist <- list()
  outlist[[1]] <- sigResults
  outlist[[2]] <- statslist
  outlist[[3]] <- sigPlots
  names(outlist) <- c("significantResults", "allRegionsStats")
  return(outlist)
}