#' @title Quick simulation of a data.frame for demonstration purposes.
#
#' @description Quick simulation of a data.frame, either with a continuous or with a binary outcome. This is merely to enable showcasing the main purpose of the predictMe package.
#
#' @param coefs Regression coefficients of a simulated model output, defaults to two predictors with coefficients 2 and 3, respectively.
#
#' @param errMean Mean prediction error, present in the simulated data, defaults to 30 (will be ignored, if function argument 'type' (see below) is set to 'binary').
#
#' @param errSD Standard deviation of the error, present in the simulated data, defaults to 3 (will be ignored, if function argument 'type' (see below) is set to 'binary').
#
#' @param intercept Intercept of a simulated model output, defaults to 1.
#
#' @param n Sample size, defaults to 1000.
#
#' @param seed A single integer value. Setting a seed ensures reproducibility of a once simulated data set.
#
#' @param type A single character value, either 'continuous' or 'binary', depending on what scale the simulated outcome shall have.
#
#' @details The returned simulated data set will have as many predictors, as the user entered regression coefficients to the function argument 'coefs'. For instance, coefs = c(.5, -2, -.9) will result in three predictors x1, x2, and x3 in the returned data set.
#'
#' The simulated data set is merely serving the need to provide the main functions of this package with the data they require (demonstration purpose; several simulation packages exist in R).
#
#' @return simDf A data.frame with one outcome column y, and as many predictor columns (named: x1, x2, …) as the user selected (default: 2). See \strong{Details}.
#
#' @author Marcel Miché
#
#' @importFrom stats var rnorm rbinom
#
#' @examples
#' # Simulate data set with continuous outcome (use all default values)
#' dfContinuous <- quickSim()
#' # Simulate data set with continuous outcome (set sample size to 149)
#' dfContinuous <- quickSim(n = 149)
#' nrow(dfContinuous) # 149
#' # Simulate data set with binary outcome (set sample size to 100, and
#' # coefficients to 3, 1, and -2.5)
#
#' @references
#'
#' Simulation code inside this function was largely taken from \href{https://stats.stackexchange.com/questions/46523/how-to-simulate-artificial-data-for-logistic-regression/46525}{Stéphane Laurent's} answer on StackExchange.
#
#' @export
#
quickSim <- function(n=1000, intercept=1, coefs=c(2, 3), errMean=30, errSD=3, seed=1, type="continuous") {
    
    # Error handling for each function argument:
    # -----------------------------------------
    # Function argument 'n':
    error.n <- tryCatch({
        errorSingleIntegerSampleSize(n)
    }, error = function(e) {
        TRUE
    })
    
    if(length(error.n)==1 && error.n) {
        message("Function argument 'n' must be an integer number > 2.\nn has been reset to its default value of 1000.\n")
        n <- 1000
    } else if(any((1:6) %in% error.n$val)) {
        message(error.n$msg, "\n")
        message("Function argument 'n' has been reset to its default value of 1000.\n")
        n <- 1000
    }
    # -----------------------------------------
    # Function argument 'intercept':
    error.intercept <- tryCatch({
        errorSingleAnyNumeric(intercept)
    }, error = function(e) {
        TRUE
    })
    
    if(length(error.intercept)==1 && error.intercept) {
        message("Function argument 'intercept' must be a single number.\nintercept has been reset to its default value of 1.\n")
        intercept <- 1
    } else if(any((1:4) %in% error.intercept$val)) {
        message(error.intercept$msg, "\n")
        message("Function argument 'intercept' has been reset to its default value of 1.\n")
        intercept <- 1
    }
    # -----------------------------------------
    # Function argument 'coefs':
    error.coefs1 <- tryCatch({
        is.vector(coefs)
    }, error = function(e) {
        FALSE
    })
    
    if(error.coefs1) {
        for(cf in 1:length(coefs)) {
            
            error.coefs2 <- tryCatch({
                errorSingleAnyNumeric(coefs[cf])
            }, error = function(e) {
                TRUE
            })
            
            if(length(error.coefs2)==1 && error.coefs2) {
                message("Function argument 'coefs' must be a vector with at least one numeric value.\ncoefs has been reset to its default vector: c(2,3).\n")
                coefs <- c(2,3)
            } else if(any((1:4) %in% error.coefs2$val)) {
                # If any of the selected coefficients are invalid, reset
                # to default vector (c(2,3)) and break out of the loop.
                message(error.coefs2$msg, "\n")
                message("Function argument 'coefs' has been reset to its default vector: c(2,3).\n")
                coefs <- c(2,3)
                break
            }
        }
    } else {
        message("Function argument 'coefs' must be a vector with at least one numeric value.\ncoefs has been reset to its default vector: c(2,3).\n")
        coefs <- c(2,3)
    }
    # -----------------------------------------
    # Function argument 'errMean':
    errMean <- 0
    error.errMean <- tryCatch({
        errorSingleAnyNumeric(errMean)
    }, error = function(e) {
        TRUE
    })
    
    if(length(error.errMean)==1 && error.errMean) {
        message("Function argument 'errMean' must be a single number.\nerrMean has been reset to its default value of 30.\n")
        errMean <- 30
    } else if(any((1:4) %in% error.errMean$val)) {
        message(error.errMean$msg, "\n")
        message("Function argument 'errMean' has been reset to its default value of 30.\n")
        errMean <- 30
    # Additional check: Must not be a negative value.
    } else if(errMean < 0) {
        message("Function argument 'errMean' must be a single number >= 0.\nerrMean has been reset to its default value of 30.\n")
        errMean <- 30
    }
    # -----------------------------------------
    # Function argument 'errSD':
    error.errSD <- tryCatch({
        errorSingleAnyNumeric(errSD)
    }, error = function(e) {
        TRUE
    })
    
    if(length(error.errSD)==1 && error.errSD) {
        message("Function argument 'errSD' must be a single number.\nerrSD has been reset to its default value of 3.\n")
        errSD <- 3
    } else if(any((1:4) %in% error.errSD$val)) {
        message(error.errSD$msg, "\n")
        message("Function argument 'errSD' has been reset to its default value of 3.\n")
        errSD <- 3
    } else if(errSD <= 0) {
        message("Function argument 'errSD' must be a single number > 0.\nerrSD has been reset to its default value of 3.\n")
        errSD <- 3
    }
    # -----------------------------------------
    # Function argument 'seed':
    error.seed <- tryCatch({
        errorSingleAnyNumeric(seed)
    }, error = function(e) {
        TRUE
    })
    
    if(length(error.seed)==1 && error.seed) {
        message("Function argument 'seed' must be a single number.\nintercept has been reset to its default value of 1.\n")
        seed <- 1
    } else if(any((1:4) %in% error.seed$val)) {
        message(error.seed$msg, "\n")
        message("Function argument 'seed' has been reset to its default value of 1.\n")
        seed <- 1
    }
    # -----------------------------------------
    # Function argument 'type':
    error.type <- tryCatch({
        errorSingleCharacter(type)
    }, error = function(e) {
        TRUE
    })
    
    if(length(error.type)==1 && error.type) {
        message("Function argument 'type' must be a character, either 'binary' or 'continuous' (lowercase).\ntype has been reset to its default 'continuous'.\n")
        type <- "continuous"
    } else if(any((1:4) %in% error.type$val)) {
        message(error.errSD$msg, "\n")
        message("Function argument 'type' has been reset to its default 'continuous'.\n")
        type <- "continuous"
    # Additional check: If type is a single character, is it correct?
    } else if(all(c(type != "binary", type != "continuous"))) {
        message("Function argument 'type' must be a character, either 'binary' or 'continuous' (lowercase).\ntype has been reset to its default 'continuous'\n")
        type <- "continuous"
    }
    # -----------------------------------------
    # Now that the function arguments are ok, run the main code.
    set.seed(seed)
    collectCoefs <- collectVals <- c()
    for(i in 1:length(coefs)) {
        # Assign values to variable x1, x2, ...
        assign(paste0("x", i), rnorm(n))
        # Collect assigned values in single vector.
        collectVals <- c(collectVals, eval(parse(text=paste0("x", i))))
        # Collect multiplication of coefficients with variables.
        collectCoefs <- c(collectCoefs, paste0(coefs[i],"*x", i))
    }
    
    # Evaluate expression of intercept and collectCoefs
    z <- eval(parse(text = paste0(c("intercept", collectCoefs), collapse = "+")))
    
    if(type=="continuous") {
        # Error term
        err <- rnorm(n=n, errMean, errSD) # error term
        # y = continuous outcome variable
        y <- z + err
    } else if(type=="binary") {
        # inverse logit function for variable z.
        pr = 1/(1+exp(-z))
        # y = binary outcome variable
        y = rbinom(n,1,pr)
    }
    # out = data frame that will be returned.
    out <- data.frame(y=y, matrix(collectVals, ncol=length(coefs), byrow = FALSE))
    # column names of predictors: "x1", "x2", ...
    colnames(out)[2:ncol(out)] <- paste0("x", 1:(ncol(out)-1))
    
    return(simDf=out)
}
