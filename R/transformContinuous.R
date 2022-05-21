# transformContinuous
#
#' @importFrom stats coef lm var
#
#
transformContinuous <- function(x=NULL, measColumn=NULL, computeRange = TRUE, range_x=c(0, 0)) {
    
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
    # If the user wants to use the range of the measured outcome values.
    if(computeRange) {
        range_x <- range(x[,measColumn], na.rm = TRUE)
    # Else: If the observed outcome values do not contain the maximum
    # value of the full range of the original scale:
    } else {
        range_x <- range_x
        
        # Error-handling for function argument range_x:
        if(!is.vector(range_x) |
           !is.numeric(range_x) |
           length(range_x)!=2 |
           range_x[1] == range_x[2]) {
            stop("The function argument 'range_x' contains an error. It must be a numeric vector with the minimum and maximum value of the original outcome scale, e.g., c(1,5).")
        }
        
        # 'Customer service': Check and turn range_x vector around, if necessary.
        if(range_x[1] > range_x[2]) {
            range_x <- range_x[2:1]
        }
    }
    
    # Error-handling for combination of function argument x and range_x (plausible?)
    if(!all(range_x == range(x[,measColumn], na.rm = TRUE))) {
        warning(paste0("The function argument 'range_x' does not concur with the actual range of values in column ", measColumn, " of the data.frame. Want to check?"))
    }
    
    # scl = scale of x after transformation.
    scl100 <- 0:100
    # range_x100 = Generate x vector of length 101 (0-100).
    range_x100 <- seq(range_x[1], range_x[2], length.out=length(scl100))
    # cf = coefficients of linear model output (intercept and weight)
    cf <- as.numeric(coef(lm(scl100 ~ range_x100)))
    
    xout <- x
    
    xout[,1] <- xout[,1]*cf[2]+cf[1]
    xout[,2] <- xout[,2]*cf[2]+cf[1]
    
    xout0 <- xout
    
    errorLs <- list()
    
    if(any(xout[,1] < 0, na.rm = TRUE)) {
        idxNegative1 <- xout[,1] < 0 & !is.na(xout[,1])
        xout[idxNegative1,1] <- 0
        errorLs[[paste0(names(x)[1], "Negative")]] <- idxNegative1
    } else if(any(xout[,1] > 100, na.rm = TRUE)) {
        idxPositive1 <- xout[,1] > 100 & !is.na(xout[,1])
        xout[idxPositive1,1] <- 100
        errorLs[[paste0(names(x)[1], "Positive")]] <- idxPositive1
    }
    
    
    if(any(xout[,2] < 0, na.rm = TRUE)) {
        idxNegative2 <- xout[,2] < 0 & !is.na(xout[,2])
        xout[idxNegative2,2] <- 0
        errorLs[[paste0(names(x)[2], "Negative")]] <- idxNegative2
    } else if(any(xout[,2] > 100, na.rm = TRUE)) {
        idxPositive2 <- xout[,2] > 100 & !is.na(xout[,2])
        xout[idxPositive2,2] <- 100
        errorLs[[paste0(names(x)[2], "Positive")]] <- idxPositive2
    }
    
    # Return linearly transformed x.
    # -----------------------------
    if(length(errorLs)!=0) {
        message("Values were detected that exceed 0 or 100. Use list output 'xTrans2' and 'idxExceed' to display these cases.")
        idxTmp <- rep(FALSE, times=nrow(x))
        for(i in 1:length(errorLs)) {
            idxTmp <- idxTmp | errorLs[[i]]
        }
        return(list(xTrans=xout, xTrans2=xout0, idxExceed=idxTmp))
    } else {
        return(list(xTrans=xout, xTrans2=xout,
                    idxExceed=rep(FALSE, times=nrow(xout))))
    }
}
