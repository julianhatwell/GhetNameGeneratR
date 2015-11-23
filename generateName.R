generateName <- function(numPhons, randomness = 1) {
  numPhons <- ifelse(numPhons < 2, 2, numPhons - 2)
  mPhons <- checkExistsMaster()
  firstPhons <- mPhons[mPhons$canBeFirst == TRUE, "phoneme"]
  lastPhons <- mPhons[mPhons$canBeLast == TRUE, "phoneme"]
  first <- sample(firstPhons, 1)
  first <- paste0(toupper(substr(first, 1, 1)), substr(first, 2, nchar(first)))
  mids <- character(0)
  if (numPhons > 0) {
    for (i in 1:numPhons) {
    mids <- paste0(mids, sample(mPhons$phoneme, 1))
    }
  }
  last <- sample(lastPhons, 1)
  paste0(first, mids, last)
}