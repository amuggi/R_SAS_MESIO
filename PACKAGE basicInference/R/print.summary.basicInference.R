#' @exportS3Method basicInference print.summary

print.summary.basicInference <-
  function(x, ...)
  {
    attr(x, "class") <- "data.frame"
    colnames(x) <- NULL
    nn <- c("Mean of sample 1", "Mean of sample 2", "Test statistic", "p-value", "")
    cat("\n")
    for (i in 1:length(x))
    {
      cat(nn[i], "\n")
      cat("----------------------------")
      print(x[i], na.print="", row.names=FALSE, quote=FALSE)
      cat("\n")
    }
  }
