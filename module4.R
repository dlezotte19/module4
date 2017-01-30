#This function will compute a 90,95,and 99% confidence Interval for a data set.
CI <- function(x) {
  if (class(x) == "numeric"){
  s <- sd(x,na.rm=T)
  n <- length(x[!is.na(x)])
  m <- mean(x,na.rm=T)
  CI90L <- m-1.645*(s/sqrt(n))
  CI90U <- m+1.645*(s/sqrt(n))
  CI95L <- m-1.96*(s/sqrt(n))
  CI95U <- m+1.96*(s/sqrt(n))
  CI99L <- m-2.576*(s/sqrt(n))
  CI99U <- m+2.576*(s/sqrt(n))
  Confidence.Intervals <- data.frame("Lower" = c(CI90L,CI95L,CI99L), "Upper" = c(CI90U,CI95U,CI99U), row.names = c("90%","95%","99%"))
  Confidence.Intervals
  } else {
  print("x must be numeric")
}
}

#Test data. The true mean is within these intervals.
data <- rnorm(22,10,6)
CI(data)

#Function accounts for NA's.
NAdata <- c(rnorm(110,40,12), rep(NA,14))
CI(NAdata)

#Function will give error message if numeric values not used
badData <- c(2,2,23,3,4,"cat","purple")
CI(badData)
