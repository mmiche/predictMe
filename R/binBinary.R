#' @title Categorization of predicted probabilities and the corresponding mean number of events for each category.
#
#' @description Predicted probabilities are categorized as bins, depending on the selected 'binWidth', and corresponding mean outcome per bin is computed.
#
#' @param binWidth A single integer value greater than 0 and less than 100, which separates 100 into equal bins, e.g., 20 (100/20 = 5 equal bins).
#
#' @param measColumn A single integer number that denotes which of the two columns of function argument 'x' contains the measured outcome.
#
#' @param x A data.frame with exactly two columns, one of the columns must be the measured outcome, the other column must be the predicted outcome values, as returned by some algorithm.
#
#' @details Predicted values (probability in percent) less than 0 or greater than 100 are replaced by 0 and 100, respectively.
#'
#' Beware: Since binning continuous values always introduces noise, some of the differences in column 7 (bin differences) require explicit attention. When the outcome is binary, the binning of the predicted probabilities (fitted values) will also automatically introduce noise in column 5, since the mean number of measured events depends on the width and on the exact borders of the bins (see package vignette, headline \strong{Bin noise}).
#
#' @return a list with two data.frames and one vector. Each data.frame has 7 columns:
#' \enumerate{
#' \item xTrans Data set, with columns 1 and 2 being categorized, according to the user's selected bin width. Each in percent, column 3 displays the observed frequencies per bin, whereas column 4 display the predicted probabilities (fitted values) per bin. Column 5 shows the difference between values in column 3 and column 4. Column 6 shows the unique individual identifiers. Column 7 shows the differences in terms of bins. See \strong{Details}.
#' \item xTrans2 Same as xTrans, only that original or transformed values less than 0 or greater than 100 have not been replaced with 0 or 100, respectively.
#' \item idxExceed logical vector. TRUE shows the row of xTrans or xTrans2 where values were either less than 0 or greater than 100.
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
#' # (estimated probability of outcome being present).
#' glmRes <- glm(y~x1+x2,data=dfBinary,family="binomial")
#' # Extract measured outcome and the predicted probability (fitted values)
#' # from the logistic regression output, put both in a data.frame.
#' glmDf <- data.frame(measOutcome=dfBinary$y,
#'                     fitted=glmRes$fitted.values)
#' # Apply function binBinary, generate 5 equal bins (probabilities in
#' # percent, bin width 20, yields 5 bins).
#' x100b <- binBinary(x=glmDf, measColumn = 1, binWidth = 20)
#
#' @export
#
binBinary <- function(x=NULL, measColumn=NULL, binWidth=20) {
    
    # Error handling for each function argument:
    # -----------------------------------------
    # Function argument 'x' (data.frame):
    if(is.null(x)) {
        stop("The function argument 'x' must be a data.frame with at least two rows and exactly two columns, with each column having a variance greater than zero.")
    }
    
    if(!(is.data.frame(x))) {
        stop("The function argument 'x' must be a data.frame with at least two rows and exactly two columns, with each column having a variance greater than zero.")
    } else {
        
        if(ncol(x) != 2) {
            stop("The function argument 'x' must have exactly two columns.")
        }
        
        if(nrow(x) < 2 |
           (var(x[,1], na.rm = TRUE) == 0 | var(x[,2], na.rm = TRUE) == 0)) {
            stop("The function argument 'x' must have two or more rows; each of both columns must have a variance greater than zero.")
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
    if(length(as.numeric(table(x[,measColumn])))!=2) {
        stop("The function argument 'measColumn' must be a single integer number (1 or 2), showing which column of the data.frame contains the measured outcome, which are expected to be the 2 categories: 0 = absent, 1 = present.")
    }
    
    # If column measColumn contains exactly two values, are these integer values?
    if(any((x[,measColumn]%%1)!=0 | is.na(x[,measColumn]))) {
        stop("The function argument 'measColumn' must be a single integer number (1 or 2), showing which column of the data.frame contains the measured outcome, which are expected to be the 2 categories: 0 = absent, 1 = present.")
    }
    
    # If column measColumn contains two integers, transform to 0 and 1:
    if(!all(range(x[,measColumn], na.rm=TRUE) == c(0,1))) {
        measColumnMax <- max(x[,measColumn], na.rm = TRUE)
        measColumnMin <- min(x[,measColumn], na.rm = TRUE)
        idxNA <- is.na(x[,measColumn])
        idxMax <- x[,measColumn] == measColumnMax & !idxNA
        x[idxMax,measColumn] <- 1
        idxMin <- x[,measColumn] == measColumnMin & !idxNA
        x[idxMin,measColumn] <- 0
    }
    
    # Check whether the other column contains fitted values (predicted probabilities) outside the expected range of min = 0 and max = 1.
    if(any(x[,-measColumn]<0) | any(x[,-measColumn]>1)) {
        message("The fitted values ('predicted probabilities') of the classification algorithm contains values either less than zero or greater than one.")
    }
    # -----------------------------------------
    # Error-handling for function argument 'binWidth':
    if(is.null(binWidth) || is.na(binWidth)) {
        stop("The function argument 'binWidth' must be a numeric vector with exactly one number, which splits 100 into even bins, e.g., 20 (100/20 = 5 even bins).")
    }
    
    if(!is.numeric(binWidth) || length(binWidth)!=1) {
        stop("The function argument 'binWidth' must be a numeric vector with exactly one number, which splits 100 into even bins, e.g., 20 (100/20 = 5 even bins).")
    }
    
    if(100%%binWidth!=0 & binWidth!=0 & binWidth!=100) {
        stop("The function argument 'binWidth' must be a numeric vector with exactly one number (> 1 and < 100), which splits 100 into even bins, e.g., 20 (100/20 = 5 even bins).")
    }
    # -----------------------------------------
    # Now that the function arguments are ok, run the main code.
    outcomeMax <- 100
    factorLevels <- seq(0, outcomeMax, binWidth)
    factorLevels <- 1:(length(factorLevels)-1)
    #
    xout <- x
    #
    errorLs <- list()
    #
    if(any(xout[,-measColumn] < 0, na.rm = TRUE)) {
        idxNegative1 <- xout[,-measColumn] < 0 & !is.na(xout[,-measColumn])
        xout[idxNegative1,-measColumn] <- 0
        errorLs[[paste0(names(x)[-measColumn], "Negative")]] <- idxNegative1
    } else if(any(xout[,-measColumn] > 1, na.rm = TRUE)) {
        idxPositive1 <- xout[,-measColumn] > 1 & !is.na(xout[,-measColumn])
        xout[idxPositive1,-measColumn] <- 1
        errorLs[[paste0(names(x)[-measColumn], "Positive")]] <- idxPositive1
    }
    # Check right away whether there is at least
    # one value that exceeds min. or max. value.
    idxExceed <- rep(FALSE, times = nrow(x))
    if(length(errorLs)!=0) {
        for(i in 1:length(errorLs)) {
            idxExceed <- idxExceed | errorLs[[i]]
        }
    }
    # anyExceed = TRUE, if at least one value
    # exceeds min. or max. value.
    anyExceed <- all(!idxExceed)
    
    # -----------------------------------------
    # Column with the glm fitted values ('predicted probabilities')
    xout[,-measColumn] <- xout[,-measColumn]*100
    xout[,-measColumn] <- cut(xout[,-measColumn], breaks=seq(from=0, to=outcomeMax, by=binWidth), labels=FALSE, include.lowest=TRUE)

    for(i in factorLevels) {
        idx_i <- xout[,-measColumn] == i & !is.na(xout[,-measColumn])
        xout[idx_i,measColumn] <- mean(xout[idx_i,measColumn], na.rm = TRUE)
    }
    # -----------------------------------------
    # Column with the observed frequencies (obsFreq) per bin of
    # the glm fitted values.
    obsFreq <- xout[,measColumn]*100
    if(anyExceed) {
        obsFreq2 <- xout[!idxExceed,measColumn]*100
    } else {
        obsFreq2 <- obsFreq
    }
    # Turn the predicted probabilities (fitted values) to percent
    fittedPerc <- x[,-measColumn]*100

    xout[,measColumn] <- cut(obsFreq, breaks=seq(from=0, to=outcomeMax, by=binWidth), labels=FALSE, include.lowest=TRUE)
    #
    absDiffBins <- abs(xout[,measColumn] - xout[,-measColumn])
    #
    # Factorize columns
    xout[,-measColumn] <- factor(xout[,-measColumn], levels = factorLevels)
    xout[,measColumn] <- factor(xout[,measColumn], levels = factorLevels)
    #
    # Add observed frequencies for each bin of glm derived
    # predicted probabilities.
    xout[,"measOutcomePerc"] <- obsFreq
    xout[,"fittedPerc"] <- fittedPerc
    xout[,"diffPerc"] <- obsFreq - fittedPerc
    xout[,"xAxisIds"] <- 1:nrow(x)
    xout[,"absDiffBins"] <- factor(absDiffBins)
    # 
    # Return results
    # --------------
    if(!anyExceed) {
        message("Values were detected that exceed 0 or 100. Use list output 'xTrans2' and 'idxExceed' to display these cases.")
        
        x[,"measOutcomePerc"] <- obsFreq2
        x[,"fittedPerc"] <- fittedPerc
        x[,"diffPerc"] <- obsFreq2 - fittedPerc
        x[,"xAxisIds"] <- 1:nrow(x)
        x[,"absDiffBins"] <- factor(absDiffBins)
        
        return(list(xTrans=xout, xTrans2=x,
                    idxExceed=idxExceed))
    } else {
        return(list(xTrans=xout, xTrans2=xout,
                    idxExceed=idxExceed))
    }
}
