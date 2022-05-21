#' @title Same as function makeDiffPlot, but add information by using colors.
#
#' @description Does the same as makeDiffPlot. However, additionally the difference between bins are added by using colors.
#
#' @param colorCol A single integer that denotes which of the columns of the data.frame contains the absolute differences between the bins of the measured and the predicted outcome.
#
#' @param idCol A single integer that denotes which of the columns of the data.frame contains the identifier of the individuals.
#
#' @param xdc A data.frame with exactly three columns, one of the columns must be the identifier of all individuals, another column must be the differences between the measured and the predicted outcome values, and the third column must be the absolute differences between the bins of the measured and the predicted outcome.
#
#' @details Recommendation: Use some of the ggplot2 options to enhance the plot, e.g., using the function facet_wrap (for an example, see vignette \strong{predictMe Why and how to?, headline 'Function makeDiffPlotColor (to go into more detail)'}).
#
#' @return a list with the plot that shows the differences between the measured and predicted outcome for all individuals, using colorized points that express the differences in terms of number of bins.
#
#' @author Marcel Miché
#
#' @importFrom ggplot2 ggplot aes_string geom_point geom_hline theme element_text
#
#' @examples
#' # Simulate data set with continuous outcome (use all default values)
#' dfContinuous <- quickSim()
#' # Use multiple linear regression as algorithm to predict the outcome.
#' lmRes <- lm(y~x1+x2,data=dfContinuous)
#' # Extract measured outcome and the predicted outcome (fitted values)
#' # from the regression output, put both in a data.frame.
#' lmDf <- data.frame(measOutcome=dfContinuous$y,
#'                    fitted=lmRes$fitted.values)
#' # Apply function binContinuous.
#' x100c <- binContinuous(x=lmDf, measColumn = 1, binWidth = 20)
#' # Apply function makeDiffPlotColor, using columns 5 and 6 from x100c[["xTrans"]]
#' # The second of columns 5 and 6 contains the identifiers of the individuals.
#' dpc <- makeDiffPlotColor(x100c[["xTrans"]][,5:7], idCol = 2, colorCol=3)
#' # dpc is the plot that shows the individual differences, in colorized form.
#' # makeDiffPlotColor works the same way if binBinary had be used instead of
#' # binContinuous.
#
#' @references
#'
#' \insertRef{ggplot2Wickham}{predictMe}
#
#' @export
#
makeDiffPlotColor <- function(xdc=NULL, idCol=NULL, colorCol=NULL) {
    
    # Error handling for each function argument:
    # -----------------------------------------
    # Function argument 'xdc' (data.frame):
    if(is.null(xdc)) {
        stop("The function argument 'xdc' must be a data.frame with at least two rows and exactly three columns (enter: ?predictMe::makeDiffPlot for details).")
    }
    
    if(!(is.data.frame(xdc))) {
        stop("The function argument 'xdc' must be a data.frame with at least two rows and exactly three columns (enter: ?predictMe::makeDiffPlot for details).")
    } else {
        
        if(ncol(xdc) != 3) {
            stop("The function argument 'xdc' must have exactly three columns (enter: ?predictMe::makeDiffPlot for details).")
        }
        
        if(nrow(xdc) < 2) {
            stop("The function argument 'xdc' must have two or more rows.")
        }
    }
    # -----------------------------------------
    # Function argument 'idCol':
    # Which of both columns contain the measured outcome values (1, 2, or 3)?
    error.idCol <- tryCatch({
        errorSingleAnyNumeric(idCol)
    }, error = function(e) {
        TRUE
    })
    
    if(length(error.idCol)==1 && error.idCol) {
        stop("Function argument 'idCol' must be a single value, either 1, 2, or 3, depending on which of these columns contains the unique IDs of the inviduals.")
    } else if(any((1:4) %in% error.idCol$val)) {
        stop("Function argument 'idCol' must be a single value, either 1, 2, or 3, depending on which of these columns contains the unique IDs of the inviduals.")
    } else if(all(c(idCol!=1, idCol!=2))) {
        stop("Function argument 'idCol' must be a single value, either 1, 2, or 3, depending on which of these columns contains the unique IDs of the inviduals.")
    }
    
    # If there are any duplicated values in idCol, stop.
    if(any(duplicated(xdc[,idCol]) & all(!is.na(xdc[,idCol])))) {
        stop("Function argument 'idCol' must contain unique IDs of the inviduals (no duplicates or missing values are permitted). For repeated measurements, consider IDs like 1a, 1b, etc.")
    }
    # -----------------------------------------
    # Function argument 'colorCol':
    # Which of both columns contain bin differences (1, 2, or 3)?
    error.colorCol <- tryCatch({
        errorSingleAnyNumeric(colorCol)
    }, error = function(e) {
        TRUE
    })
    
    if(length(error.colorCol)==1 && error.colorCol) {
        stop("Function argument 'colorCol' must be a single value, either 1, 2, or 3, depending on which of these columns contains the differences between the bins.")
    } else if(any((1:4) %in% error.colorCol$val)) {
        stop("Function argument 'colorCol' must be a single value, either 1, 2, or 3, depending on which of these columns contains the differences between the bins.")
    } else if(all(c(colorCol!=1, colorCol!=2, colorCol!=3))) {
        stop("Function argument 'colorCol' must be a single value, either 1, 2, or 3, depending on which of these columns contains the differences between the bins.")
    }
    
    # The other column is expected to contain the
    # differences between observed and predicted outcome.
    xdcNms <- colnames(xdc) # Nms = Names
    xdc[,xdcNms[colorCol]] <- factor(xdc[,xdcNms[colorCol]])
    pDiff <-
        ggplot2::ggplot(data = xdc,
                        ggplot2::aes_string(
                            x=xdcNms[idCol],
                            y=xdcNms[-c(idCol, colorCol)],
                            group=1)) +
        ggplot2::geom_point(ggplot2::aes_string(
            color=xdcNms[colorCol])) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", size=1) +
        ggplot2::theme(
            axis.text = ggplot2::element_text(size=14),
            axis.title = ggplot2::element_text(size=14))
    
    return(list(diffPlotColor=pDiff))
}
