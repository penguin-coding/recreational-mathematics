trim <- function(character) substr(character, 1, 1)

leadingDigitPlot <- function(dataVector){
  strings <- as.character(dataVector)
  leadingDigits <- sapply(strings, trim)
  leadingDigits <- as.integer(leadingDigits)
  hist(leadingDigits)
}

data <- runif(10000, 0, 1100)
data <- round(data, 2)
leadingDigitPlot(data)