#' Conducts a t test and gets the sample means, test statistic and p-value
#'
#' @param sample1 Vector with observations from sample1.
#' @param sample2 Vector with observations from sample2.
#' @param cl Confidence level.
#' @return A vector with sample means, test statistic and p-value.
#' @examples
#' s1 <- rnorm(500)
#' s2 <- rnorm(500) 
#' t_test(s1, s2)

t_test <- function(sample1, sample2, cl=0.95)
{
  m1 <- mean(sample1)
  m2 <- mean(sample2)
  s  <- t.test(sample1, sample2)$statistic
  p  <- t.test(sample1, sample2)$p.value
  if (p < (1-cl)) frase <- "Means are significantly different"
  if (p >= (1-cl)) frase <- "Means are not significantly different"
  res <- c(m1, m2, s, p, frase)
  class(res) <- "basicInference" 
  attr(res, "sample1") <- sample1 
  attr(res, "sample2") <- sample2 
  return(res)
}

