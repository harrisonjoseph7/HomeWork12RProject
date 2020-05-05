#' Run Linear Regression Analysis for a Created Dataset
#' 
#' This function allows someone to create a covariate matrix with a set of outcomes and run linear regression analysis
#' 
#' @param x A covariate Matrix 
#' @param y A Vector of Outcomes
#' @param sub A list of Subjects (integers corresponding to each row in matrix x)
#' @return 
#' @export
#' @examples 
#' myLinearRegression(x,y,sub)
#' myLinearRegression(xvalues, yvalues, subjects)

myLinearRegression <- function(x,y,sub){
  
  my_plotter <- if(ncol(x)<5){
    GGally::ggpairs(x, title = "Correlagram of Cars Sales by Year in Each Season")  
  } else if(ncol(x)>5){
    print("Too many variables to plot")
  }
  
  y2 <- c(y[sub])
  x2 <- as.matrix(x[sub,])
  mod1 <- lm(y2~x2)
  Summary_Model <- summary(mod1)$coef
  
  return(list("coef"=Summary_Model[,1], "pval"=Summary_Model[,4], my_plotter)) 
  
}

