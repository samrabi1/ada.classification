#' @title 'kmeansPlus'
#' @description A tool to aid in the implementation of conventional k means clustering, kmeansPlus() provides the normal functionality of stats::kmeans() with some modest additions. In short, it allows the user to quickly plot results from kmeans(), as well as add cluster identifiers to the input dataframe as a new variable.
#' @param data A matrix or dataframe containing numeric data in tidy format. This is equivalent to the 'x' argument from kmeans().
#' @param k Either the desired number of clusters, or a set of initial (distinct) cluster centers. This is equivalent to the 'centers' argument from kmeans().
#' @param exclude A vector, either numeric or character, indicating either indices or colnames of variables in 'data' that the user wishes to exclude. The excluded variable will not be removed from the tibble printed in the final output (when full.output and plot are set to FALSE), but it will be excluded from all clustering and vizualisation procedures.
#' @param full.output A logical vector indicating whether the output of kmeansPlus() should include the full output of kmeans().
#' @param plot A logical vector indicating whether the output of kmeansPlus() should include a plot.
#' @param x A string (character vector of length 1) containing the colname of a variable from `data` to be plotted on the x axis (if plot = TRUE).
#' @param y A string (character vector of length 1) containing the colname of a variable from `data` to be plotted on the y axis (if plot = TRUE).
#' @param iter.max This is equivalent to the 'iter.max' argument from kmeans(), and is set to the same default.
#' @param nstart This is equivalent to the 'nstart' argument from kmeans(), and is set to the same default.
#' @param algorithm This is equivalent to the 'algorithm' argument from kmeans(), and is set to the same default.
#' @param trace This is equivalent to the 'trace' argument from kmeans(), and is set to the same default.
#' @keywords classification, k means
#' @export
#' @examples
#' kmeansPlus(data = iris, k = 3, exclude = "Species", full.output = TRUE, plot = TRUE,
#' x = "Sepal.Length", y = "Sepal.Width")

kmeansPlus <- function(data, k, exclude = NULL, full.output = TRUE, plot = TRUE, x = NULL, y = NULL, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), trace = FALSE)
{

# Coercing `data` to class tibble and removing any variables named in `exclude`
  data <- dplyr::as_tibble(data)
  data1 <- dplyr:: select(data, -exclude)

# Running kmeans
  output0 <- stats::kmeans(x = data1, centers = k, iter.max = iter.max, nstart = nstart, algorithm = algorithm, trace = trace)

# Adding a new variable to `data` indicating which cluster each case was assigned to by the kmeans procedure
  cluster <- (output0$cluster)
  output1 <- dplyr::mutate(data, cluster)


# Creating the plot, with x and y axes according to variables named in the arguments `x` and `y` and with points colored according to their cluster
  Cluster <- factor(cluster)
  if(plot == TRUE){
  output2 <- ggplot2::ggplot(data1, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = Cluster)) + ggplot2::geom_point() + ggplot2::xlab(colnames(data[, x])) + ggplot2::ylab(colnames(data[, y]))
}


# Determining which outputs to print according to the arguments `full.output` and `plot`
if (full.output == TRUE & plot == TRUE) {
  final <- list(output0, output2)} else {

if (full.output == TRUE & plot == FALSE) {
  final <- output0} else {

if (full.output == FALSE & plot == TRUE) {
  final <- output2} else{

if (full.output == FALSE & plot == FALSE) {
  final <- output1}
}}}


final
}
