#' @title Categorization of measured and predicted outcome values.
#
#' @description Measured and predicted continuous outcome values (transformed to range between 0 and 100) are categorized as bins, depending on the selected 'binWidth'.
#
#' @param binWidth A single integer value greater than 0 and less than 100, which separates 100 into equal bins, e.g., 20 (100/20 = 5 equal bins).
#
#' @param computeRange Logical value, defaults to TRUE, meaning that the range of the column with the measured outcome values will be computed. Else set this argument to FALSE (see \strong{Details}).
#
#' @param measColumn A single integer number that denotes which of the two columns of function argument 'x' contains the measured outcome.
#
#' @param range_x A vector with the minimum and maximum possible value of the continuous outcome scale (see \strong{Details}).
#
#' @param x A data.frame with exactly two columns, one of the columns must be the measured outcome, the other column must be the predicted outcome values, as returned by some algorithm.
#
#' @details Regarding function arguments 'computeRange' and 'range_x': If either the minimum or maximum possible value of the outcome scale has not occurred, e.g., none of the participants selected the maximum possible answer option, then the user must pass the possible range of outcome values to this function, using the function argument 'range_x', e.g., range_x = c(1, 5), if the original outcome scale ranged from 1 to 5.
#'
#' Regarding function output 'xTrans' (see \strong{Value}): Predicted values less than 0 or greater than 100 are replaced by 0 and 100, respectively.
#'
#' Beware: The differences in column 5 are as accurate (no information loss) as if the original measured and predicted outcome values were subtracted from one another. However, since binning continuous values always introduces noise, some of the differences in column 7 (bin differences) require explicit attention (see package vignette, headline \strong{Bin noise}).
#
#' @return a list with two data.frames and one vector. Each data.frame has 7 columns:
#' \enumerate{
#' \item xTrans Data set, with columns 1 and 2 being categorized, according to the user's selected bin width. Column 3 displays the observed outcome values, whereas column 4 displays the predicted outocme values (fitted values), both transformed to range between 0 and 100. Column 5 shows the difference between values in column 3 and column 4. Column 6 shows the unique individual identifiers. Column 7 shows the differences in terms of bins. See \strong{Details}.
#' \item xTrans2 Same as xTrans, only that original or transformed values less than 0 or greater than 100 have not been replaced with 0 or 100, respectively.
#' \item idxExceed logical vector. TRUE shows the row of xTrans or xTrans2 where values were either less than 0 or greater than 100.
#' }
#
#' @author Marcel Miché
#
#' @importFrom utils head
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
#' # Apply function binContinuous, generate 5 equal bins (transformed
#' # outcome 0-100, bin width = 20, yields 5 bins).
#' x100c <- binContinuous(x=lmDf, measColumn = 1, binWidth = 20)
#
#' @export
#
binContinuous <- function(x=NULL, measColumn=NULL, binWidth=20, computeRange = TRUE, range_x=c(0, 0)) {
    
    # Run helper function 'transformContinuous'
    # Except for 'binWidth', all other function arguments
    # of function 'binContinuous' are checked by this
    # helper function, as to whether they throw errors.
    xTransLs <- transformContinuous(
        x=x, measColumn = measColumn,
        computeRange = computeRange, range_x=range_x)
    
    # Add difference and xAxis Ids for plotting
    # later on, if plotting will be selected.
    xTransLs[["xTrans"]][,"diff"] <-
        xTransLs[["xTrans"]][,measColumn] -
        xTransLs[["xTrans"]][,-measColumn]
    xTransLs[["xTrans"]][,"xAxisIds"] <- 1:nrow(x)
    
    # Do the same with xTransLs[["xTrans2"]]:
    xTransLs[["xTrans2"]][,"diff"] <-
        xTransLs[["xTrans2"]][,measColumn] -
        xTransLs[["xTrans2"]][,-measColumn]
    xTransLs[["xTrans2"]][,"xAxisIds"] <- 1:nrow(x)
    
    # names(xTransLs)
    # 
    head(xTransLs[["xTrans"]])
    head(xTransLs[["xTrans2"]])
    all(!xTransLs[["idxExceed"]])
    
    # Error handling for remaining function argument
    # binWidth:
    # ----------------------------------------------
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
    if(measColumn==1) {
        xTmp <- xTransLs[["xTrans"]][,1:2]
        xTmp2 <- xTransLs[["xTrans2"]][,1:2]
    } else if(measColumn==2) {
        xTmp <- xTransLs[["xTrans"]][,2:1]
        xTmp2 <- xTransLs[["xTrans2"]][,2:1]
    }
    
    outcomeMax <- 100
    factorLevels <- seq(0, outcomeMax, binWidth)
    factorLevels <- 1:(length(factorLevels)-1)
    
    # Collect levels (in numeric format) in list
    levelDiff <- list()
    for(i in 1:2) {
        xTmp[,i] <- cut(xTmp[,i], breaks=seq(from=0, to=outcomeMax, by=binWidth), labels=FALSE, include.lowest=TRUE)
        levelDiff[[i]] <- xTmp[,i]
        # Factorize column
        xTmp[,i] <- factor(xTmp[,i], levels = factorLevels)
    }
    #
    absBinDiff <- abs(levelDiff[[1]]-levelDiff[[2]])
    
    # Copy    
    xTmp2 <- xTmp
    
    colnames(xTmp) <- colnames(xTmp2) <-
        paste0(colnames(xTmp), "Bin")
    
    xTransLs[["xTrans"]] <-
        data.frame(xTransLs[["xTrans"]], xTmp)
    # Re-order columns, so that the order matches
    # the output of the 'binBinary' function.
    xTransLs[["xTrans"]] <- xTransLs[["xTrans"]][,c(5,6,1:4)]
    
    xTransLs[["xTrans2"]] <-
        data.frame(xTransLs[["xTrans2"]], xTmp2)
    # Do the same with xTransLs[["xTrans2"]]:
    xTransLs[["xTrans2"]] <- xTransLs[["xTrans2"]][,c(5,6,1:4)]
    
    #
    xTransLs[["xTrans"]][,"absBinDiff"] <- factor(absBinDiff)
    xTransLs[["xTrans2"]][,"absBinDiff"] <- factor(absBinDiff)
    
    # Insert NAs in all rows that contain values
    # that exceeded either min. or max. values.
    xTmp2[xTransLs[["idxExceed"]],] <- NA
    
    # anyExceed = TRUE, if at least one value
    # exceeds min. or max. value.
    noExceed <- all(!xTransLs[["idxExceed"]])
    
    # Return results
    # --------------
    if(!noExceed) {
        message("Values were detected that exceed 0 or 100. Use list output 'xTrans2' and 'idxExceed' to display these cases.")
    }
    # 
    return(list(xTrans=xTransLs[["xTrans"]],
                xTrans2=xTransLs[["xTrans2"]],
                idxExceed=xTransLs[["idxExceed"]]))
}
