#' @exportS3Method basicInference plot

plot.basicInference <-
  function(x, ...)
  {
    if (!inherits(x, "basicInference")) stop("Wrong object")
    plot(attr(x, "sample1"), attr(x, "sample2"), ...)
  }
