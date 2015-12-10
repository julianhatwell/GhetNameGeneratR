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

includePhon <- function(liked, out) {
  if (length(liked) != length(out)) {
    stop("liked and out parameters must be same length")
  }
  n <- length(out)
    # check liked and out have same length
  # use length as sample size
  p <- rbeta(n, liked + 25, out + 25)
  b <- as.logical(rbinom(n, 1, p))
  return(b)
}

generateName <- function(phonLength = 3, gen = "u", randomness = 1) {
  checkGenderAssignment(gen)
  gender <- genderAssignment(gen)
  numPhons <- ifelse(phonLength < 2, 2, phonLength - 2)
  mPhons <- getMasterPhonemes()
  firstPhons <- mPhons[mPhons$canBeFirst & includePhon(mPhons$likedFirst, mPhons$outFirst), "phoneme"]
  if (gen == "u") lastPhons <- mPhons[(mPhons$maleEnding | mPhons$femaleEnding) & includePhon(mPhons$likedMale + mPhons$likedFemale, mPhons$outMale + mPhons$outFemale), "phoneme"]
  if (gen == "m") lastPhons <- mPhons[mPhons$maleEnding & includePhon(mPhons$likedMale, mPhons$outMale), "phoneme"]
  if (gen == "f") lastPhons <- mPhons[mPhons$femaleEnding & includePhon(mPhons$likedFemale, mPhons$outFemale), "phoneme"]
  
  first <- sample(firstPhons, 1)
  currentOutFirst <- mPhons[mPhons$phoneme == first, "outFirst"]
  mPhons[mPhons$phoneme == first, "outFirst"] <- currentOutFirst + 1
  first <- paste0(toupper(substr(first, 1, 1)), substr(first, 2, nchar(first)))
  middle <- mPhons[mPhons$canBeMid & includePhon(mPhons$likedMid, mPhons$outMid), "phoneme"]
  phonList <- character(0)
  mids <- character(0)
  if (numPhons > 0) {
    for (i in 1:numPhons) {
      mid <- sample(middle, 1)
      currentOutMid <- mPhons[mPhons$phoneme == mid, "outMid"]
      mPhons[mPhons$phoneme == mid, "outMid"] <- currentOutMid + 1
      mids <- paste0(mids, mid)
      phonList <- c(phonList, mid)
    }
  }
  last <- sample(lastPhons, 1)
  currentOutMale <- mPhons[mPhons$phoneme == last, "outMale"]
  currentOutfemale <- mPhons[mPhons$phoneme == last, "outFemale"]
  newOutMale <- ifelse(gen %in% c("u", "m"),  currentOutMale + 1, currentOutMale)
  newOutFemale <- ifelse(gen %in% c("u", "f"), currentOutfemale + 1, currentOutfemale)
  mPhons[mPhons$phoneme == last, c("outMale", "outFemale")] <- c(newOutMale, newOutFemale)
  
  gName <- list()
  class(gName) <- "ghetName"
  gName$name <- paste0(first, mids, last)
  gName$phonList <- c(first, phonList, last)
  gName$phonLength <- phonLength
  gName$gender <- gen
  gName$like <- as.logical(NA)
  
  # commit the ratings updates
  commitPhons(mPhons)
  
  return(gName)
}

saveName <- function(sN) {
  if (is.na(sN$like)) sN$like <- FALSE
  if (sN$like) {
    mPhons <- getMasterPhonemes()
    
    currentLikedFirst <- mPhons[mPhons$phoneme == tolower(sN$phonList[1]), "likedFirst"]
    mPhons[mPhons$phoneme == tolower(sN$phonList[1]), "likedFirst"] <- currentLikedFirst + 1
    
    currentLikedMale <- mPhons[mPhons$phoneme == tolower(sN$phonList[sN$phonLength]), "likedMale"]
    currentLikedfemale <- mPhons[mPhons$phoneme == tolower(sN$phonList[sN$phonLength]), "likedFemale"]
    newLikedMale <- ifelse(sN$gender %in% c("u", "m"), currentLikedMale + 1, currentLikedMale)
    newLikedFemale <- ifelse(sN$gender %in% c("u", "f"), currentLikedfemale + 1, currentLikedfemale)
    mPhons[mPhons$phoneme == sN$phonList[sN$phonLength], c("likedMale", "likedFemale")] <- c(newLikedMale, newLikedFemale)
    
    if (sN$phonLength > 2) {
      for (mid in sN$phonList[-c(1, sN$phonLength)]) {
        currentLikedMid <- mPhons[mPhons$phoneme == mid, "likedMid"]
        mPhons[mPhons$phoneme == mid, "likedMid"] <- currentLikedMid + 1
      }
    }
    commitPhons(mPhons)
  }
}