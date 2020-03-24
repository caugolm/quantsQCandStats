#' Make a plot of a bunch of ROI stats from a linear model, per your specifications
#' 
#' @param plotList A list object with 1st element a stats table organized as long in terms of Label.Names x term (from your lm output)
#'    The 2nd element is a data frame with columns "value", "label", "Label.Names", "resid", plus whatever you stuck into your lm()
#'    The width in the df should be organized as estimate, std.error, statistic, and p.value
#'    Needs raw values ("value"), as well as columns associated with each term in your regression
#'    Note that you can do this with only a single row, or a few select rows, but then don't use the function's multiple corrections tests
#' @param termForSignificance the term from your df you want to use for your stats. Must be from the "term" column of plotList[[1]]
#' @param x what you want on your x-axis. If it's a factor or character, this makes a boxplot. If it's a numeric, double, or integer, it's a scatterplot.
#' @param y what you want on your y-axis. Almost certainly "value" or "resid" 
#' @param interactionPlot (logical) are you plotting an interaction effect? Should only be used when considering an interaction effect statistically. Default FALSE 
#' @param boxOrScatter "box" or "scatter" depending on how you want to visualize your data
#' @param colorCol what you want to color by
#' @param colorMat (optional) table of colors that might get used in plots. If used, might fight with colorCol. I need to practice
#' @param sigValue value used for thresholding your statistics. If you want to show all data use 1 (default).
#' @param multipleCorrectionMethod (optional) goes into p.adjust() to correct for multiple comparisons. Will do this accounting for all labels in your input df. Default is none, common alternatives are "bonferroni" and "fdr"
#' @param outFileRoot (optional) Not implemented. file root for output. If specified, will save 1) .txt file with all significant results and 2) plots for up to 5 labels (with 5 smallest p-values)
#' @return A list object with elements 
#'     1) a plot for each significant region per your specifications
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#' @export

runLmsPlots <- function (plotList, termToPlot, x, y, interactionPlot = FALSE, boxOrScatter = "box", colorCol, colorMat = NULL, sigValue = 1, multipleCorrectionMethod = "none", outFileRoot = NULL) {
  statsmat <- plotList[[1]]
  df <- plotList[[2]]
  
 #  # y needs to be a number type for now
 #  dttmp <- unlist(select(df, y))
 #  ytype <- typeof(dttmp) 
 #  if (ytype != "numeric" & ytype != "double" & ytype != "integer") { 
 #    stop("y must be a numeric, double, or integer") 
 #  }
 #  
 #  # i'm sure this will end up edited
 #  dxtmp <- unlist(select(df, x))
 #  xtype <- typeof(dxtmp) 
 # # print(xtype)
 #  if (xtype == "numeric" | xtype == "double" | xtype == "integer") { 
 #    plottype <- "scatter"
 #  } else {
 #    plottype <- "box"
 #  }

    if (boxOrScatter == "scatter") { 
      plottype <- "scatter"
    } else {
      plottype <- "box"
    }
  
  # make a default color mat for plotting
  if (is.null(colorMat)) { 
    numColors <- dim(unique(select(df, colorCol)))[1]
    print(numColors)
    colorMat <- RColorBrewer::brewer.pal(numColors, "Set1")
  }
  
  # threshold your DF by whatever term specified 
  statsmat <- filter(statsmat, term == termToPlot) 
  
  # we may want to correct for multiple comparisons. If specified in function input, do that now:
  statsmat$p_adj <- p.adjust(statsmat$p.value, multipleCorrectionMethod)
  
  # threshold your ROIs by whatever threshold specified 
  sigstats <- filter(statsmat, p_adj < sigValue) 

  # order stats from smallest p-value to largest
  sigstats <- sigstats[with(sigstats,order(p_adj)),]
  
  # filter for just significant regions 
  sigdf <- filter(df, label %in% sigstats$label)
  
  if(dim(sigdf)[1] == 0 ) { 
    stop("Nothing significant!")
  }
  
  allThePlots <- list()
  print(y)
  yy <- ensym(y)
  print(yy)
  xx <- ensym(x)
  cc <- ensym(colorCol)
  if ( plottype == "scatter"){ 
    for (i in 1:length(sigstats$label)) {
      lbl <- sigstats$label[i]
      lblname <- sigstats$Label.Names[i]
      if (interactionPlot == TRUE ) { 
        p <- ggplot(sigdf[sigdf$label == lbl , ], aes(x = x, y = y, color = !!cc)) + geom_point() # + ggtitle(lblname) + scale_color_manual(values=colorMat) + geom_smooth(method="lm")
      } else {
        p <- ggplot(sigdf[sigdf$label == lbl , ], aes(x = !!xx, y = !!yy, color = !!cc)) + geom_point() + ggtitle(lblname) + scale_color_manual(values=colorMat) + geom_smooth(method="lm" ,aes(group=1))
      }
      allThePlots[[i]] <- p
    }
  } else {
    for (i in 1:length(sigstats$label)) {
      print("trying boxplots")
      print(typeof(sigdf))
      print(xx)
      print(yy)
      lbl <- sigstats$label[i]
      lblname <- sigstats$Label.Names[i]
      p <- ggplot(sigdf[sigdf$label == lbl , ], aes(x = x, y = !!yy, fill = !!cc)) + geom_boxplot() + geom_jitter(height=0, width = .15) + ggtitle(lblname) + scale_fill_manual(values=colorMat)
      allThePlots[[i]] <- p
    } 
  }
  return(allThePlots)
}

