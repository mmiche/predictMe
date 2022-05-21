#' @title Return of five common results, based on the 2x2 cross-table (a.k.a. confusion matrix).
#
#' @description Upon receiving two binary variables (only 0 and 1 permitted) of equal length, return sensitivity, specificity, positive predictive value, negative predictive value, and the base rate of the outcome.
#
#' @param measColumn A single integer number that denotes which of the two columns of function argument 'x' contains the measured outcome.
#
#' @param print2by2 Logical value, defaults to FALSE. If set TRUE, two 2by2 matrices will be printed with explanations of what they display.
#
#' @param xr A data.frame with exactly two columns, one of the columns must be the binary measured outcome, the other column must be the binary predicted outcome, based on some algorithm's predictions (see \strong{Details}).
#
#' @details The r in the argument 'xr' stands for response, meaning that the predicted probabilities must have been transformed to a binary outcome, usually by using the default cutoff of 0.5; although it may also be any other cutoff between 0 and 1.
#'
#' If you wish to additionally print the 2x2 matrix, set the argument 'print2by2' TRUE (default: FALSE).
#
#' @return a list with five elements (seven, if argument print2by2 is set TRUE; see \strong{Details}):
#' \enumerate{
#' \item sens Sensitivity (a.k.a.: Recall, True Positive Rate).
#' \item spec Specificity (a.k.a.: True Negative Rate).
#' \item ppv Positive Predictive Value (a.k.a.: Precision).
#' \item npv Negative Predictive Value.
#' \item br Base rate of the outcome (mean outcome occurrence in the sample).
#' \item tbl1 2x2 matrix. Test-theoretic perspective: Specificity in top left cell, sensitivity in bottom right cell.
#' \item tbl2 2x2 matrix. Test-practical perspective (apply test in the real world): Negative predictive value (npv) in top left cell, positive predictive value (ppv) in bottom right cell.
#' }
#
#' @author Marcel Miché
#
#' @importFrom stats var
#
#' @examples
#' # Simulate data set with binary outcome
#' dfBinary <- quickSim(type="binary")
#' # Logistic regression, used as algorithm to predict the response variable
#' # (response = estimated probability of outcome being present).
#' glmRes <- glm(y~x1+x2,data=dfBinary,family="binomial")
#' # Extract measured outcome and the predicted probability (fitted values)
#' # from the logistic regression output, put both in a data.frame.
#' glmDf <- data.frame(measOutcome=dfBinary$y,
#'                     fitted=glmRes$fitted.values)
#' # binary outcome, based on the default probability threshold of 0.5.
#' get2by2Df <- data.frame(
#'     measuredOutcome=glmDf$measOutcome,
#'     predictedOutcome=ifelse(glmDf$fitted<.5, 0, 1))
#' # Demand 2x2 matrix to be part of the resulting list.
#' my2x2 <- get2by2(xr=get2by2Df, measColumn=1, print2by2 = TRUE)
#' # Display both 2x2 matrices
#' # tbl1: Theoretical perspective, with specificity in top left cell,
#' # sensitivity in bottom right cell.
#' my2x2$tbl1
#' # tbl2: Practical perspective, with negative predictive value (npv)
#' # in top left cell, positive predictive value (ppv) in bottom right
#' # cell.
#' my2x2$tbl2
#
#' @export
#
get2by2 <- function(xr, measColumn=NULL, print2by2=FALSE) {
    
    # Error handling for each function argument:
    # -----------------------------------------
    # Function argument 'xr' (data.frame):
    if(is.null(xr)) {
        stop("The function argument 'xr' must be a data.frame with at least two rows and exactly two columns, with each column having a variance greater than zero.")
    }
    
    if(!(is.data.frame(xr))) {
        stop("The function argument 'xr' must be a data.frame with at least two rows and exactly two columns, with each column having a variance greater than zero.")
    } else {
        
        if(ncol(xr) != 2) {
            stop("The function argument 'xr' must have exactly two columns.")
        }
        
        if(nrow(xr) < 2 |
           (var(xr[,1], na.rm = TRUE) == 0 | var(xr[,2], na.rm = TRUE) == 0)) {
            stop("The function argument 'xr' must have two or more rows; each of both columns must have a variance greater than zero.")
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
    # -----------------------------------------
    # Check whether the column with the original binary outcome
    # really contains only two values.
    if(length(as.numeric(table(xr[,measColumn])))!=2) {
        stop("The function argument 'measColumn' must be a single integer number (1 or 2), showing which column of the data.frame contains the measured outcome, which are expected to be the 2 categories: 0 = absent, 1 = present.")
    }
    
    # If column measColumn contains exactly two values, are these integer values?
    if(any((xr[,measColumn]%%1)!=0 | is.na(xr[,measColumn]))) {
        stop("The function argument 'measColumn' must be a single integer number (1 or 2), showing which column of the data.frame contains the measured outcome, which are expected to be the 2 categories: 0 = absent, 1 = present.")
    }
    
    # If column measColumn contains two integers, transform to 0 and 1:
    if(!all(range(xr[,measColumn], na.rm=TRUE) == c(0,1))) {
        measColumnMax <- max(xr[,measColumn], na.rm = TRUE)
        measColumnMin <- min(xr[,measColumn], na.rm = TRUE)
        idxNA <- is.na(xr[,measColumn])
        idxMax <- xr[,measColumn] == measColumnMax & !idxNA
        xr[idxMax,measColumn] <- 1
        idxMin <- xr[,measColumn] == measColumnMin & !idxNA
        xr[idxMin,measColumn] <- 0
    }
    # -----------------------------------------
    # Function argument 'print2by2':
    error.print2by2 <- tryCatch({
        errorSingleLogical(print2by2)
    }, error = function(e) {
        TRUE
    })
    
    if(all(c(print2by2!=TRUE, print2by2!=FALSE))) {
        stop("Function argument 'print2by2' must be either TRUE or FALSE.")
    }
    
    yFitted <- xr[,-measColumn]
    yMeasured <- xr[,measColumn]
    tbl <- table(yFitted, yMeasured)
    
    if(print2by2) {
        
        tbl1 <- prop.table(table(yFitted, yMeasured), margin = 2)
        tbl2 <- prop.table(table(yFitted, yMeasured), margin = 1)
        
        return(list(sens=tbl[2,2]/sum(tbl[,2]),
                    spec=tbl[1,1]/sum(tbl[,1]),
                    ppv=tbl[2,2]/sum(tbl[2,]),
                    npv=tbl[1,1]/sum(tbl[1,]),
                    baseRate=sum(tbl[,2])/sum(tbl),
                    tbl1=tbl1,
                    tbl2=tbl2))
        
    }
    return(list(sens=tbl[2,2]/sum(tbl[,2]),
                spec=tbl[1,1]/sum(tbl[,1]),
                ppv=tbl[2,2]/sum(tbl[2,]),
                npv=tbl[1,1]/sum(tbl[1,]),
                baseRate=sum(tbl[,2])/sum(tbl)))
}
