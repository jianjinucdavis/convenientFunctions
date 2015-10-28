
#' recode a continous vector into a factor vector of two categories by median
#' @param x a numeric vector
#' @return a factor vector of two levels: low and high
#' @export
cut2byMedian <- function(x){
  stopifnot(is.numeric(x))
  x2 = character(length(x))
  x2[x <= median(x)] <- "Low"
  x2[x > median(x)] <- "High"
  x2factor <- as.factor(x2)
  return(x2factor)
}

#' recode a continous vector into a factor vector of three categories by quantile
#' @param x a numeric vector
#' @return a factor vector of three levels: low, mid, high
#' @export

cut3byQuantile <- function(x){
  stopifnot(is.numeric(x))
  x3 = character(length(x))
  x3[x < as.numeric(quantile(x))[2]] <- "Low"
  x3[x >= as.numeric(quantile(x))[2] & x <= as.numeric(quantile(x))[4]] <- "Mid"
  x3[x > as.numeric(quantile(x))[4]] <- "High"
  x3factor <- as.factor(x3)
  return(x3factor)
}

#' recode a continous vector into a factor vector of three categories by sd
#' @param x a numeric vector
#' @return a factor vector of three levels: low, mid, high
#' @export

cut3bySD <- function(x){
  stopifnot(is.numeric(x))
  x3 = character(length(x))
  x3[x < as.numeric(mean(x) - sd(x))] <- "Low"
  x3[x >= as.numeric(mean(x) - sd(x)) & x <= as.numeric(mean(x) + sd(x))] <- "Mid"
  x3[x > as.numeric(mean(x) + sd(x))] <- "High"
  x3factor <- as.factor(x3)
  return(x3factor)
}

#' recode a continous vector into a factor vector of three categories by low 1/3, mid 1/3, high 1/3
#' @param x a numeric vector
#' @return a factor vector of three levels: low, mid, high
#' @export

cut3equal <- function(x){
  stopifnot(is.numeric(x))
  x3 = character(length(x))
  x3[x < unname(quantile(x, 0.33))] <- "Low"
  x3[x >=  unname(quantile(x, 0.33)) & x <= unname(quantile(x, 0.67))] <- "Mid"
  x3[x > unname(quantile(x, 0.67))] <- "High"
  x3factor <- as.factor(x3)
  return(x3factor)
}

