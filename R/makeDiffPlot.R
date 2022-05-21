#' @title Plot individual differences between measured and predicted outcome values.
#
#' @description Plot the differences between measured and predicted outcome for all individuals.
#
#' @param idCol A single integer that denotes which of the columns of the data.frame contains the identifier of the individuals.
#
#' @param xd A data.frame with exactly two columns, one of the columns must be the identifier of all individuals, the other column must be the differences between the measured and the predicted outcome values.
#
#' @details The d in 'xd' stands for differences, meaning that the column of interest contain the differences between the measured and the predicted outcome values, logically requiring the column that identifies the individuals.
#'
#' Irrespective of whether the original outcome was continuous or binary, outcome values always range between 0 and 100. For instance, for a binary outcome the 'probabilities' are represented as percentage.
#'
#' Use the column diff (from function \code{\link{binContinuous}}) or diffPerc (from function \code{\link{binBinary}}) and column xAxisIds, both columns being part of both data.frames that are returned by the two mentioned functions.
#
#' @return a list with the plot that shows the differences between the measured and predicted outcome for all individuals. See \strong{Details}.
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
#' # Apply function makeDiffPlot, using columns 5 and 6 from x100c[["xTrans"]]
#' # The second of columns 5 and 6 contains the identifiers of the individuals.
#' dp <- makeDiffPlot(x100c[["xTrans"]][,5:6], idCol = 2)
#' # dp is the plot that shows the individual differences.
#' # makeDiffPlot works the same way if binBinary had be used instead of
#' # binContinuous.
#
#' @references
#'
#' \insertRef{ggplot2Wickham}{predictMe}
#
#' @export
#
makeDiffPlot <- function(xd=NULL, idCol=NULL) {

    # Error handling for each function argument:
    # -----------------------------------------
    # Function argument 'xd' (data.frame):
    if(is.null(xd)) {
        stop("The function argument 'xd' must be a data.frame with at least two rows and exactly two columns, with each column having a variance greater than zero.")
    }

    if(!(is.data.frame(xd))) {
        stop("The function argument 'xd' must be a data.frame with at least two rows and exactly two columns.")
    } else {

        if(ncol(xd) != 2) {
            stop("The function argument 'xd' must have exactly two columns.")
        }

        if(nrow(xd) < 2) {
            stop("The function argument 'xd' must have two or more rows.")
        }
    }
    # -----------------------------------------
    # Function argument 'idCol':
    # Which of both columns contain the measured outcome values (1 or 2)?
    error.idCol <- tryCatch({
        errorSingleAnyNumeric(idCol)
    }, error = function(e) {
        TRUE
    })

    if(length(error.idCol)==1 && error.idCol) {
        stop("Function argument 'idCol' must be a single value, either 1 (if the first column contains the unique IDs of the inviduals) or 2 (if the second column contains the unique IDs of the inviduals.")
    } else if(any((1:4) %in% error.idCol$val)) {
        stop("Function argument 'idCol' must be a single value, either 1 (if the first column contains the unique IDs of the inviduals) or 2 (if the second column contains the unique IDs of the inviduals.")
    } else if(all(c(idCol!=1, idCol!=2))) {
        stop("Function argument 'idCol' must be a single value, either 1 (if the first column contains the unique IDs of the inviduals) or 2 (if the second column contains the unique IDs of the inviduals.")
    }

    # If there are any duplicated values in idCol, stop.
    if(any(duplicated(xd[,idCol]) & all(!is.na(xd[,idCol])))) {
        stop("Function argument 'idCol' must contain unique IDs of the inviduals (no duplicates or missing values are permitted). For repeated measurements, consider IDs like 1a, 1b, etc.")
    }

    # The other column is expected to contain the
    # differences between observed and predicted outcome.
    xdNms <- colnames(xd) # Nms = Names
    pDiff <-
        ggplot2::ggplot(data = xd,
                        ggplot2::aes_string(
                            x=xdNms[idCol],
                            y=xdNms[-idCol],
                            group=1)) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", size=1) +
        ggplot2::theme(
            axis.text = ggplot2::element_text(size=14),
            axis.title = ggplot2::element_text(size=14))

    return(list(diffPlot=pDiff))
}
