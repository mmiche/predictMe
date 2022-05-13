# errorSingleAnyNumeric
#
#
errorSingleAnyNumeric <- function(try_x) {
    # 4 error cases. The last exit case means: None of the checked errors applied.
    errorCases <- 4
        # 1
    if(length(try_x)!=1) {
        return(
            list(val=1,
                 msg="Function argument requires a single value.",
                 ec=errorCases))
        # 2
    } else if(is.na(try_x)) {
        return(
            list(val=2,
                 msg="Function argument must not be NA.",
                 ec=errorCases))
        # 3 
    } else if(is.null(try_x)) {
        return(
            list(val=3,
                 msg="Function argument must not be NULL.",
                 ec=errorCases))
        # 4 
    } else if(!is.numeric(try_x)) {
        return(
            list(val=4,
                 msg="Function argument must be either of class integer or of class numeric.",
                 ec=errorCases))
    } else {
        return(
            list(val=0,
                 msg="errorSingleAnyNumeric filed no complaint.",
                 ec=errorCases))
    }
}
