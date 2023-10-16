#' linreg_ref_class.R
#'
#' Reference class for the function linreg
#'
#' @field formula The formula used to fit the regression.
#' @field input_arg_data The input used for the data argument.
#' @field coefficients Coefficients for the regression model. 
#' @field fitted_values Fitted values for the model.
#' @field residuals Residuals for the model.
#' @field df  Degrees of freedoms for the model.
#' @field residual_variance The variance of the residuals.
#' @field coefficients_variance Covariance matrix of the regression coefficients.
#' @field t_values t-values for the coefficients.
#' @field p_values p-values for the coefficients. 
#' 
#' @import methods
#' @export linreg
#' @exportClass linreg
linreg <- setRefClass("linreg",
                      fields = list(formula = "formula",
                                    input_arg_data = "character",
                                    coefficients = "numeric",
                                    fitted_values = "numeric",
                                    residuals = "numeric",
                                    df = "numeric",
                                    residual_variance = "numeric",
                                    coefficients_variance = "matrix",
                                    t_values = "numeric",
                                    p_values = "numeric",
                                    plot_data = "data.frame",
                                    outliers = "data.frame"),
                      methods = list(
                        initialize = function(formula, data){
                          if(!base::inherits(formula, "formula")){
                            stop ("First input is not a formula")
                          }
                          if(!base::is.data.frame(data)){
                            stop ("Data needs to be a data frame")
                          }
                          input_vars <- base::all.vars(formula)
                          if(!base::length(base::intersect(input_vars, base::colnames(iris))) == base::length(input_vars)){
                            stop("Variables in formula does not exist in data frame")
                          } 
                          formula <<- formula
                          input_arg_data <<- base::deparse1(base::substitute(data))
                          X <- stats::model.matrix(formula, data)
                          y <- data[[base::all.vars(formula)[1]]]
                          coef <- base::solve(t(X) %*% X) %*% t(X) %*% y
                          fitted_values <<- base::as.vector(X %*% coef)
                          residuals <<- y - fitted_values
                          scale_res <- base::scale(residuals)
                          df <<- base::nrow(X) - base::length(coef)
                          residual_variance <<- base::drop(base::t(residuals) %*% residuals) / (df)
                          coefficients_variance <<- residual_variance * base::solve(base::t(X) %*% X)
                          t_values <<- base::as.vector(coef / (base::sqrt(base::diag(coefficients_variance))))
                          p_values <<- base::as.vector(2 * stats::pt(base::abs(t_values), df, lower.tail = FALSE))
                          coef_names <- base::row.names(coef)
                          coefficients <<- base::as.vector(coef)
                          names(coefficients) <<- coef_names
                          names(residuals) <<- 1:base::length(residuals)
                          names(fitted_values) <<- 1:base::length(fitted_values)
                          plot_data <<-  base::data.frame(res = residuals,
                                                          fitted_val = fitted_values,
                                                          scale_res = scale_res)
                          outliers <<- plot_data[base::abs(scale_res) > 2, ]
                        })
)

#' Print function for linreg object.
#'
#' Print method for the reference class linreg.
#'
#' @name linreg_print
#' @return A printout of the function call and coefficients from the regression model. 
#' 
linreg$methods(
  print = function(){
    cat("Call:\n", 
        "linreg(formula = ", format(formula),
        ", data = ", format(input_arg_data), ")\n",
        "\n",
        "Coefficients:\n",
        sep="")
    base::print(round(coefficients,3))
  })


#' Residuals for linreg object.
#'
#' Residuals from the regression model of reference class linreg.
#'
#' @name linreg_resid
#' @return A named vector with the residuals from the regression model. 
#' 
linreg$methods(resid = function(){base::return(residuals)})



#' Predicted values for linreg object.
#'
#' Fitted values from the regression model of reference class linreg.
#'
#' @name linreg_pred
#' @return A named vector with the fitted values from the regression model. 
#' 
linreg$methods(pred = function(){base::return(fitted_values)})



#' Coefficients for linreg object.
#'
#' The estimated coefficients from the regression model for linreg objects. 
#'
#' @name linreg_coef
#' @return A print of the coefficents from the regression model. 
#' 
linreg$methods(coef = function(){base::return(coefficients)})



#' Summary for linreg object.
#'
#' A short summary of a linreg object. 
#'
#' @name linreg_summary
#' @return A print of the coefficents from the regression model with their 
#' respective standard deviation, t-values, and p-values.
#' The models residual standard error and degrees of freedom are also printed.
#' 
linreg$methods(summary = function(){
  temp_pvalue <- rep(" ", length(p_values))
  temp_pvalue[temp_pvalue < 0.1] <- "."
  temp_pvalue[temp_pvalue < 0.05] <- "*"
  temp_pvalue[temp_pvalue < 0.01] <- "**"
  temp_pvalue[temp_pvalue < 0.001] <- "***"
  coef_print <- data.frame(base::round(coefficients, 5),
                           base::round(base::sqrt(base::diag(coefficients_variance)), 5),
                           base::round(t_values, 3),
                           base::signif(p_values, 3),
                           temp_pvalue)
  colnames(coef_print) <- c("Estimate", "Std. error", "t value", "Pr(>|t|)", "")
  base::cat("Coefficients:\n")
  base::print(coef_print)
  base::cat("---\n",
            "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n",
            "Residual standard error: ",
            base::round(base::sqrt(residual_variance), 4),
            " on ",
            df,
            " degrees of freedom.",
            sep="")
})



#' Plot function for linreg object.
#'
#' Produces a residual vs fitted plot and a scale-location plot.
#' The scale-location plot shows square roof of standardized residuals vs fitted values. 
#'
#' @name linreg_summary
#' @return Two different ggplot objects. 
#' 
linreg$methods(plot = function(){
  current_plot <- 1  # Initialize the current plot counter
  repeat{
    if (current_plot == 2) {
      # Plot 1, Residuals vs Fitted
      base::print(ggplot2::ggplot(data = plot_data,
                            ggplot2::aes(fitted_val ,res)) +
              ggplot2::geom_point(shape = 1, size = 3) +
              ggplot2::labs(x = "Fitted values",
                            y = "Residuals",
                            title = "Residuals vs Fitted") +
              ggplot2::geom_smooth(formula = y ~ x,
                                   method = "loess",
                                   color = "red", 
                                   se = FALSE,
                                   span = 1.2) +
              ggplot2::geom_hline(ggplot2::aes(yintercept=median(res)),
                                  color = "lightgrey", 
                                  linetype = "dotted") +
              ggplot2::geom_text(data = outliers,
                                 ggplot2::aes(x =fitted_val, 
                                              y = res, 
                                              label = rownames(outliers)),
                                 vjust = -0.5,
                                 hjust = -0.5, 
                                 color = "black") +
              ggplot2::labs(x = "Fitted Values", 
                            y = "Residuals", 
                            title = "Residuals vs. Fitted") +
              ggplot2::theme_bw())
      
    }  else if(current_plot == 3) {
      # Plot 2, Scale−Location
      base::print(ggplot2::ggplot(data = plot_data, 
                            ggplot2::aes(fitted_val, sqrt(abs(scale_res)))) +
              ggplot2::geom_point(shape = 1, size = 3) +
              ggplot2::labs(x = "Fitted values",
                            y = "Residuals",
                            title = "Residuals vs Fitted") +
              #  ggplot2::stat_summary(fun = median, geom = "line", color = "red", size = 1) +
              ggplot2::geom_smooth(formula = y ~ x,
                                   method = "loess",
                                   color = "red", 
                                   se = FALSE, 
                                   span = 1.5) +
              ggplot2::geom_hline(ggplot2::aes(yintercept=median(sqrt(abs(scale_res)))),
                                  color = "lightgrey",
                                  linetype = "dotted") +
              ggplot2::geom_text(data = outliers,
                                 ggplot2::aes(x =fitted_val,
                                              y = sqrt(abs(scale_res)),
                                              label = rownames(outliers)),
                                 vjust = -0.5,
                                 hjust = -0.5,
                                 color = "black") +
              ggplot2::labs(x = "Fitted Values",
                            y = "Square root of Standardized residuals",
                            title = "Scale-Location") +
              ggplot2::theme_bw())
    } 
    
    if (current_plot == 3) {
      break  # Exit the loop if the users hit return 2 times
    }
    input <- readline(prompt = "Hit <Return> to see next plot:")
    # Increment the current plot counter and cycle back to the first plot if necessary
    current_plot <- current_plot + 1
  }
})


