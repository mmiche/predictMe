#' @title Tabularize the essential result of the predictMe package.
#
#' @description Provides the essential result of the predictMe package, three tables, and, optionally, two plots.
#
#' @param measColumn A single integer number that denotes which of the two columns of function argument 'x' contains the measured outcome.
#
#' @param plot Logical value, defaults to FALSE. If set TRUE, two complementary plots will be part of the list that this function returns.
#
#' @param plotCellRes Logical value, defaults to TRUE (is ignored if function argument 'plot' is set FALSE). If set FALSE, the heatmap is returned without frequency results in the cellls.
#
#' @param xc A data.frame with exactly two columns, one of the columns must be the categorized measured outcome, the other column must be the categorized predicted outcome.
#
#' @details The c in 'xc' stands for categorized, meaning that the outcome values are expected to have been categorized, so that both columns contain the exact same categories, and are of the class factor.
#'
#' Columns 1 and 2 of the output 'xTrans' from function \code{\link{binBinary}} and from function \code{\link{binContinuous}} provide the expected input of this \code{makeTablePlot} function (see \strong{Examples}).
#'
#' The returned list will contain 7 items, if function argument 'plot' is set TRUE, if FALSE, it will return the first 5 items (see \strong{Values}).
#
#' @return a list with five or seven items (see \strong{Details}):
#' \enumerate{
#' \item totalCountTable A table with the total counts.
#' \item rowSumTable A table with proportions that sum up to 1, per row (summing across columns).
#' \item colSumTable A table with proportions that sum up to 1, per column (summing across rows).
#' \item rowSumTable_melt The rowSumTable, reformated by the function melt of the reshape2 package.
#' \item colSumTable_melt The colSumTable, reformated by the function melt of the reshape2 package.
#' \item rowSumTable_plot The rowSumTable_melt data, plotted by the function ggplot of the ggplot2 package.
#' \item colSumTable_plot The colSumTable_melt data, plotted by the function ggplot of the ggplot2 package.
#' }
#
#' @author Marcel Miché
#
#' @importFrom ggplot2 ggplot aes_string geom_tile geom_text scale_fill_continuous theme element_text
#' @importFrom reshape2 melt
#' @importFrom stats var
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
#' # Apply function binBinary
#' x100c <- binContinuous(x=lmDf, measColumn = 1, binWidth = 20)
#' # Apply function makeDiffPlot, using columns 1 and 2 from x100c[["xTrans"]]
#' # The first of columns 1 and 2 contains the measured outcome values.
#' tp <- makeTablePlot(x100c[["xTrans"]][,1:2], measColumn = 1, plot = TRUE)
#' # tp is a list with 7 items, items 6 and 7 are the plots that represent
#' # the numeric information of items 2 and 3 (and 4 and 5, which merely have
#' # a different format).
#' # Display item 6 (plot no.1). Perfect performance if the diagonal cells all
#' # contain the value 1.
#' tp$rowSumTable_plot
#
#' @references
#'
#' \insertRef{ggplot2Wickham}{predictMe}
#'
#' \insertRef{reshape2Wickham}{predictMe}
#
#' @export
#
makeTablePlot <- function(xc=NULL, measColumn=NULL, plot=FALSE, plotCellRes=TRUE) {
    
    # Error handling for each function argument:
    # -----------------------------------------
    # Function argument 'x' (data.frame):
    if(is.null(xc)) {
        stop("The function argument 'xc' must be a data.frame with at least two rows and exactly two columns, with each column having a variance greater than zero.")
    }
    
    if(!(is.data.frame(xc))) {
        stop("The function argument 'xc' must be a data.frame with at least two rows and exactly two columns, with each column having a variance greater than zero.")
    } else {
        
        if(ncol(xc) != 2) {
            stop("The function argument 'xc' must have exactly two columns.")
        }
        
        if(nrow(xc) < 2 |
           (var(as.numeric(xc[,1]), na.rm = TRUE) == 0 | var(as.numeric(xc[,2]), na.rm = TRUE) == 0)) {
            stop("The function argument 'xc' must have two or more rows; each of both columns must have a variance greater than zero.")
        }
    }
    # -----------------------------------------
    # Function argument 'measColumn':
    # Which of both columns contain the measured outcome values (1 or 2)?
    error.measColumn <- tryCatch({
        errorSingleAnyNumeric(measColumn)
    }, error = function(e) {
        TRUE
    })
    
    if(length(error.measColumn)==1 && error.measColumn) {
        stop("Function argument 'measColumn' must be a single value, either 1 (if the first column contains the measured outcome) or 2 (if the second column contains the measured outcome.")
    } else if(any((1:4) %in% error.measColumn$val)) {
        stop("Function argument 'measColumn' must be a single value, either 1 (if the first column contains the measured outcome) or 2 (if the second column contains the measured outcome.")
    } else if(all(c(measColumn!=1, measColumn!=2))) {
        stop("Function argument 'measColumn' must be a single value, either 1 (if the first column contains the measured outcome) or 2 (if the second column contains the measured outcome.")
    }
    
    # Both columns must be factors
    for(i in 1:2) {
        if(!is.factor(xc[,i])) {
            stop(paste0("Column ", i, " is not a factor. Both columns of the data.frame must be factors, having the same levels."))
        }
    }
    # Both columns must have the exact same categories.
    if(nlevels(xc[,1])!=nlevels(xc[,2])) {
        stop("Both columns of the data.frame must have the same number of levels.")
    } else if(!all(levels(xc[,1]) == levels(xc[,2]))) {
        stop("Both columns of the data.frame must have the same levels.")
    }
    # -----------------------------------------
    # Function argument 'plot':
    error.plot <- tryCatch({
        errorSingleLogical(plot)
    }, error = function(e) {
        TRUE
    })
    
    if(all(c(plot!=TRUE, plot!=FALSE))) {
        stop("Function argument 'plot' must be either TRUE or FALSE.")
    }
    # -----------------------------------------
    # Function argument 'plotCellNum':
    if(plot==TRUE) {
        error.plotCellRes <- tryCatch({
            errorSingleLogical(plotCellRes)
        }, error = function(e) {
            TRUE
        })
        
        if(all(c(plotCellRes!=TRUE, plotCellRes!=FALSE))) {
            stop("Function argument 'plotCellRes' must be either TRUE or FALSE.")
        }
    }
    # -----------------------------------------
    # Catch column name of measure outcome
    columnNameOfMeasured <- names(xc)[measColumn]
    if(is.null(columnNameOfMeasured)) {
        colnames(xc)[measColumn] <- "measOutcome"
    }
    # Catch column name of predicted outcome
    columnNameOfPrediction <- names(xc)[-measColumn]
    if(is.null(columnNameOfPrediction)) {
        colnames(xc)[-measColumn] <- "fitted"
    }
    # -----------------
    # Total counts
    allTbl <- table(xc[,-measColumn], xc[,measColumn],
                    dnn = list(columnNameOfPrediction,
                               columnNameOfMeasured))
    # -----------------
    # Perspective 1 (sum across columns)
    rowSumTbl <- prop.table(
        table(xc[,-measColumn], xc[,measColumn],
              dnn = list(columnNameOfPrediction,
                         columnNameOfMeasured)),
        # Rows sum up to 1 (sum across columns).
        margin = 1)
    # If there are rows or columns with only
    # NAs (meaning no entries), replace with 0.
    rowSumTbl[is.na(rowSumTbl)] <- 0
    # -----------------
    # Perspective 2 (sum across rows)
    colSumTbl <- prop.table(
        table(xc[,-measColumn], xc[,measColumn],
              dnn = list(columnNameOfPrediction,
                         columnNameOfMeasured)),
        # Columns sum up to 1 (sum across rows).
        margin = 2)
    # If there are rows or columns with only
    # NAs (meaning no entries), replace with 0.
    colSumTbl[is.na(colSumTbl)] <- 0
    #
    # -----------------------------------------
    # Perspective 1 - melt -----------------
    meltedRowSumTbl <- reshape2::melt(rowSumTbl)
    meltedRowSumTbl[,"valueDigits2"] <- round(meltedRowSumTbl$value, digits = 2)
    # Factorize column 1
    meltedRowSumTbl[,columnNameOfMeasured] <-
        factor(meltedRowSumTbl[,columnNameOfMeasured], levels = levels(xc[,1]))
    # Factorize column 2
    meltedRowSumTbl[,columnNameOfPrediction] <-
        factor(meltedRowSumTbl[,columnNameOfPrediction], levels = rev(levels(xc[,2])))
    #
    # Perspective 2 - melt -----------------
    meltedColSumTbl <- reshape2::melt(colSumTbl)
    meltedColSumTbl[,"valueDigits2"] <- round(meltedColSumTbl$value, digits = 2)
    # Factorize column 1
    meltedColSumTbl[,columnNameOfMeasured] <-
        factor(meltedColSumTbl[,columnNameOfMeasured], levels = levels(xc[,1]))
    # Factorize column 2
    meltedColSumTbl[,columnNameOfPrediction] <-
        factor(meltedColSumTbl[,columnNameOfPrediction], levels = rev(levels(xc[,2])))
    #
    if(plot) {
        
        # Perspective 1 - plot -----------------
        pRowSumTbl <-
            ggplot2::ggplot(
                data = meltedRowSumTbl,
                ggplot2::aes_string(
                    x=columnNameOfMeasured,
                    y=columnNameOfPrediction,
                    fill="value")) +
            ggplot2::geom_tile() +
            ggplot2::scale_fill_continuous(
                high = "#132B43",
                low = "#56B1F7") +
            ggplot2::theme(legend.position = "top",
                  axis.text =
                      ggplot2::element_text(size=14),
                  axis.title =
                      ggplot2::element_text(size=14))
        if(plotCellRes) {
            pRowSumTbl <- pRowSumTbl +
                ggplot2::geom_text(
                    ggplot2::aes_string(
                        x=columnNameOfMeasured,
                        y=columnNameOfPrediction,
                        label = "valueDigits2"),
                    color = "white", size = 4)
        }
        # Perspective 2 - plot -----------------
        pColSumTbl <-
            ggplot2::ggplot(
                data = meltedColSumTbl,
                ggplot2::aes_string(
                    x=columnNameOfMeasured,
                    y=columnNameOfPrediction,
                    fill="value")) +
            ggplot2::geom_tile() +
            ggplot2::scale_fill_continuous(
                high = "#132B43",
                low = "#56B1F7") +
            ggplot2::theme(legend.position = "top",
                           axis.text =
                               ggplot2::element_text(size=14),
                           axis.title =
                               ggplot2::element_text(size=14))
        if(plotCellRes) {
            pColSumTbl <- pColSumTbl +
                ggplot2::geom_text(
                    ggplot2::aes_string(
                        x=columnNameOfMeasured,
                        y=columnNameOfPrediction,
                        label = "valueDigits2"),
                    color = "white", size = 4)
        }
        return(list(
            totalCountTable=allTbl,
            rowSumTable=rowSumTbl,
            colSumTable=colSumTbl,
            rowSumTable_melt=meltedRowSumTbl,
            colSumTable_melt=meltedColSumTbl,
            rowSumTable_plot=pRowSumTbl,
            colSumTable_plot=pColSumTbl))
    } else {
        
        return(list(
            totalCountTable=allTbl,
            rowSumTable=rowSumTbl,
            colSumTable=colSumTbl,
            rowSumTable_melt=meltedRowSumTbl,
            colSumTable_melt=meltedColSumTbl))
    }
}
