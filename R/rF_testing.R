#' @title A random forest classifier training and testing function
#'
#' @description This function allows one to perform random forest classification (see (\code{\link[randomForest]{randomForest}}) for more information regarding the function to perform this classification method) of varying models on a training and testing set of the original data, the size of which is defined by the user.
#'
#' @param dat dataset to perform test on
#' @param s proportion of dataset to be assigned to the 'training' dataset
#' @param x the classifying variable in the dataset
#' @param model written in the style of a regression, defines the classifying variable and the predictors the user wants to use to perform the test
#'
#' @keywords classification, random forest
#'
#' @export
#'
#' @examples
#' rF_testing(dat = iris, s = 0.5, x = Species, model = Species ~ Petal.Length + Petal.Width + Sepal.Length)

rF_testing <- function(dat, s, x, model) {

  train <- dplyr::sample_frac(dat, s)
  test <- dplyr::anti_join(dat, train)

  random <- randomForest::randomForest(model, data=train)

  test_mat <- stats::predict(random, newdata = test)
  attach(test)
  table <- table(x, test_mat)
  detach(test)

  list <- list(random$confusion, table)


  return(list)

}
