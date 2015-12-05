print.ghetName <- function (gN) {
  gen <- ifelse(gN$gender == "f", "girl's"
                , ifelse(gN$gender == "m", "boy's"
                         , "unisex"))
  output <- cat("Your name is:", gN$name
              ,"\n\nThis is a", gen, "name")
  invisible(output)
}


"[<-.ghetName" <- function(gN, subscript, value) {
  gN$phonList[subscript] <- value
  gN$name <- paste0(gN$phonList, collapse = "")
  print(gN)
  return(gN)
}

generateName <- function(phonLength, gen = "u", randomness = 1) {
  checkGenderAssignment(gen)
  gender <- genderAssignment(gen)
  numPhons <- ifelse(phonLength < 2, 2, phonLength - 2)
  mPhons <- getMasterPhonemes()
  firstPhons <- mPhons[mPhons$canBeFirst == TRUE, "phoneme"]
  lastPhons <- mPhons[mPhons$canBeLast == TRUE &
                        mPhons$male == gender["m"] &
                        mPhons$female == gender["f"]
                        , "phoneme"]
  first <- sample(firstPhons, 1)
  first <- paste0(toupper(substr(first, 1, 1)), substr(first, 2, nchar(first)))
  phonList <- character(0)
  mids <- character(0)
  if (numPhons > 0) {
    for (i in 1:numPhons) {
      mid <- sample(mPhons$phoneme, 1)
      mids <- paste0(mids, mid)
      phonList <- c(phonList, mid)
    }
  }
  last <- sample(lastPhons, 1)
  
  gName <- list()
  class(gName) <- "ghetName"
  gName$name <- paste0(first, mids, last)
  gName$phonList <- c(first, phonList, last)
  gName$phonLength <- phonLength
  gName$gender <- gen
  gName$like <- as.logical(NA)
  
  return(gName)
}