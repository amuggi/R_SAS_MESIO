#' @exportS3Method basicInference summary

summary.basicInference <-
  function(object, ...)
  {
    if(!inherits(object, "basicInference")) stop("Wrong data type")
    ans <- data.frame(object[1], object[2], object[3], object[4], object[5])
    class(ans)  <- "summary.basicInference"
    return(ans)
  }
