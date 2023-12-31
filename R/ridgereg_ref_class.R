
#' Reference class for the function ridgereg
#'
#' @field formula The formula used to fit the regression.
#' @field input_arg_data The input used for the data argument.
#' @field coefficients Coefficients for the regression model. 
#' @field fitted_values Fitted values for the model.
#' @field lambda Penalty parameter for the ridge regression.
#' @field coef_names Names of the coefficients from the ridge regression.
#' @field mean_X Mean value for each of the dependent variables.
#' @field sd_X Standard deviation for each of the dependent variables.
#' @field mean_y Mean value for the dependent variable. 
#' 
#' @import methods
#' @export ridgereg
#' @exportClass ridgereg
ridgereg <-
  setRefClass("ridgereg",
              fields = list(
                formula = "formula",
                input_arg_data = "character",
                coefficients = "numeric",
                fitted_values = "numeric",
                lambda = "numeric",
                coef_names = "character",
                mean_X = "numeric",
                sd_X = "numeric",
                mean_y = "numeric"
              ),
              methods = list(
                initialize = function(formula, data, lambda=0){
                  # Does not estimate intercept
                  X <- stats::model.matrix(formula, data)[,-1]
                  y <- data[[base::all.vars(formula)[1]]]
                  mean_X <<- apply(X, 2, mean)
                  sd_X <<- apply(X, 2, sd)
                  for(i in 1:length(mean_X)){
                    X[, i] <- (X[, i] - mean_X[i]) / sd_X[i]
                  }
                  coef <- base::solve( (t(X)%*%X) + lambda*diag(dim(X)[2]) ) %*% t(X) %*% y
                  mean_y <<- mean(y)
                  formula <<- formula
                  input_arg_data <<- base::deparse1(base::substitute(data))
                  coef_names <<- c("(Intercept)", base::row.names(coef))
                  coefficients <<- as.vector(coef)
                  lambda <<- lambda
                  fitted_values <<- as.vector(X %*% coef + mean_y)
                }
              )
  )

#' Predict function for ridgereg object.
#'
#' Predict new values of observation with the existing ridge regression model.
#'
#' @name ridgereg_predict
#' @return A vector of the predicted values.
#' 
ridgereg$methods(
  predict = function(data){
    if(!is.data.frame(data)){
      stop("Data needs to be a data frame.")
    }
    # formula contains dependent variable and data does not.
    if(! (length(colnames(data)) == (length(all.vars(formula))-1) )){
      stop("The data used to predict does not match the data the model used.")
    }
    if(!all (colnames(data) %in% all.vars(formula) ) ){
      stop("The column names of the data for prediction does not match the data
           used to estimate the model.")
    }
      
     
    formula1 <- all.vars(formula)[-1]
    formula1 <- paste(formula1, collapse=" + ")
    formula1 <- paste("~", formula1, sep="")
    data <- stats::model.matrix(as.formula(formula1), data)[,-1]

    # Scale the new data with the models mean and sd
    data <- as.matrix(data)
    for(i in 1:length(mean_X)){
      data[, i] <- (data[, i] - mean_X[i]) / sd_X[i]
    }
    coef_temp <- coefficients/sd_X
    intercept <- mean_y - sum(coef_temp*mean_X) 
    
    fitted_values <<- as.vector(data %*% as.matrix(coefficients) + mean_y)
    return(fitted_values)
  }
)


#' Coefficients for ridgereg object.
#'
#' The estimated coefficients from the regression model for ridgereg objects. 
#'
#' @name ridgereg_coef
#' @return A print of the coefficents from the ridge regression model. 
#' 
ridgereg$methods(coef = function(){return(coefficients)})


#' Print function for ridgereg object.
#'
#' Print method for the reference class ridgereg.
#'
#' @name ridgereg_print
#' @return A printout of the function call and coefficients from the regression model. 
#' 
ridgereg$methods(
  print = function(){
    coef_temp <- coefficients/sd_X
    intercept <- mean_y - sum(coef_temp*mean_X) 
    coef_print <- c(intercept, coef_temp)
    names(coef_print) <- coef_names
    cat("Call:\n", 
        "linreg(formula = ", format(formula),
        ", data = ", format(input_arg_data), ")\n",
        "\n",
        "Coefficients:\n",
        sep="")
    base::print(round(coef_print,3))
  })





#' Show function for ridgereg object.
#'
#' Show method for the reference class ridgereg.
#'
#' @name ridgereg_print
#' @return A printout of the function call and coefficients from the regression model. 
#' 
ridgereg$methods(
  show = function(){
    coef_temp <- coefficients/sd_X
    intercept <- mean_y - sum(coef_temp*mean_X) 
    coef_print <- c(intercept, coef_temp)
    names(coef_print) <- coef_names
    cat("Call:\n", 
        "linreg(formula = ", format(formula),
        ", data = ", format(input_arg_data), ")\n",
        "\n",
        "Coefficients:\n",
        sep="")
    base::print(round(coef_print,3))
  })


