#' @title Add individual new predictor and outcome values (for demonstration purposes).
#
#' @description Add single predictor and outcome values. This function is for demonstration purposes, especially in terms of visualizing how far from perfect prediction a new outcome value may be.
#
#' @param binWidth A single integer value greater than 0 and less than 100, which separates 100 into equal bins, e.g., 20 (100/20 = 5 equal bins).
#
#' @param newData A data.frame with exactly as many columns (and the same column names) as the data set to which the linear model has been fitted (see \code{linearModel}).
#
#' @param linearModel The linear model that has been fitted to a data set (see \strong{Details}).
#
#' @details A linear regression model is the easiest model to use for the demonstration purposes of this function.
#
#' @return a data.frame with four columns:
#' \enumerate{
#' \item newObservedOutcome New observed outcome, linearly transformed to a variable that ranges between 0 and 100.
#' \item newPrediction New predicted outcome, linearly transformed to a variable that ranges between 0 and 100.
#' \item diff Difference between newObservedOutcome and newPrediction.
#' \item binDiff Absolute difference in terms of bins (see \code{binWidth}).
#' }
#
#' @author Marcel Miché
#
#' @importFrom stats coef lm predict.lm
#
#' @examples
#' # Simulate data set with continuous outcome (use all default values)
#' dfContinuous <- quickSim()
#' # Use multiple linear regression as algorithm to predict the outcome.
#' lmRes <- lm(y~x1+x2,data=dfContinuous)
#' # Set new predictor values and observed outcome values for 2 individuals.
#' newData <- data.frame(x2=c(-.2, .25),
#'                    x1=c(.3, .4),
#'                    y=c(5, 8))
#' # Execute the function 'addContinuous'.
#' addContinuous(linearModel=lmRes, binWidth = 20, newData=newData)
#
#' @export
#
addContinuous <- function(linearModel=NULL, binWidth=20,
                          newData=NULL) {
    
    # 
    if(class(linearModel)!="lm") {
        stop("The function arument 'linearModel' must be a linear model which has been fitted to the data, using the lm() function from the stats package in R.")
    }
    
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
    
    # 
    if(class(newData)!="data.frame") {
        stop("The function argument 'newData' must be of class data.frame. The column names must be equal to the column names of the data to which the linear model has been fitted (see function argument 'linearModel').")
    }
    
    previousData <- linearModel$model
    
    predictorNames <- attr(linearModel$terms, "term.labels")
    matchedColNames <- match(colnames(previousData), predictorNames)
    predictorIdx <- which(!is.na(matchedColNames))
    outcomeIdx <- which(is.na(matchedColNames))
    
    # 
    if(!all(colnames(previousData) %in% colnames(newData))) {
        stop("The column names of the function argument 'newData' must be equal to the column names of the data to which the linear model has been fitted (see function argument 'linearModel').")
    }
    
    # 
    predictorIdxNew <- colnames(newData) %in% colnames(previousData)[predictorIdx]
    
    # Check whether the new predictor values are within the range of the predictor values in the data to which the linear model has been fitted.
    # cnd = colnamesNewData
    colnamesNewData <- colnames(newData)[predictorIdxNew]
    # Check every predictor column in 'newData'
    for(cnd in colnamesNewData) {
        range_cndTmp <- range(previousData[,cnd])
        outsideTmp <- newData[,cnd] < range_cndTmp[1] | newData[,cnd] > range_cndTmp[2]
        if(any(outsideTmp)) {
            exceedVals <- as.character(paste0(which(outsideTmp), collapse = ", "))
            warning(paste0("New predictor values in column ", cnd, " exceed the range of values in the data set that was used to fit the model to the data. See row number(s) ", exceedVals, "."))
        }
    }
    
    newPredIndiv <- predict.lm(linearModel, newdata = newData[,predictorIdxNew])
    
    range_x <- range(previousData[,outcomeIdx])
    
    # scl = scale of x after transformation.
    scl100 <- 0:100
    # range_x100 = Generate x vector of length 101 (0-100).
    range_x100 <- seq(range_x[1], range_x[2], length.out=length(scl100))
    # cf = coefficients of linear model output (intercept and weight)
    cf <- as.numeric(coef(lm(scl100 ~ range_x100)))
    
    scl100lm <- lm(scl100 ~ range_x100)
    
    newDataIn <- data.frame(newObservedOutcome=newData[,!predictorIdxNew],
                            newPrediction=newPredIndiv)
    
    newDataOut <- newDataIn
    
    newDataOut[,1] <- predict.lm(object = scl100lm,
                                 newdata = data.frame(range_x100=newDataIn[,1]))
    
    newDataOut[,2] <- predict.lm(object = scl100lm,
                                 newdata = data.frame(range_x100=newDataIn[,2]))
    
    # 
    newDataOut$diff <- newDataOut[,1]-newDataOut[,2]
    
    # Do not use values 1 to 101, be
    newBinsCol1 <-
        cut(c(scl100, as.numeric(newDataOut[,1])),
            breaks = 100/binWidth, labels=FALSE)[-c(1:101)]
    newBinsCol2 <-
        cut(c(scl100, as.numeric(newDataOut[,2])),
            breaks = 100/binWidth, labels=FALSE)[-c(1:101)]
    
    newDataOut$binDiff <- abs(newBinsCol1-newBinsCol2)
    
    return(newDataOut)
}
