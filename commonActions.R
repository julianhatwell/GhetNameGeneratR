getMasterPhonemes <- function() GhetNameEnv$MasterPhonemes

commitPhons <- function(phonDF) {
  GhetNameEnv$MasterPhonemes <<- phonDF[order(phonDF$phoneme),]
}

genderAssignment <- function(gender) {
  m <- if(gender %in% c("m", "u")) TRUE else FALSE
  f <- if(gender %in% c("f", "u")) TRUE else FALSE
  return(c(m = m,f = f))
}

makePhoneme <- function(phon, fst, mid, gen) {
  # common validation
  mPhons <- getMasterPhonemes()
  checkFirstAndMid(fst, mid)
  checkGenderAssignment(gen)
  checkExistsPhoneme(phon, mPhons$phoneme)
  
  #process steps
  gender <- genderAssignment(gen)
  newPhons <- rbind(mPhons, c(tolower(phon), fst, mid, gender["m"], gender["f"], rep(0,8)))
  commitPhons(newPhons)
  print(paste("Added phoneme ", phon, " to Master Phonemes File"))
}

updatePhoneme <- function(phon, fst, mid, gen) {
  # common validation
  mPhons <- getMasterPhonemes()
  checkFirstAndMid(fst, mid)
  checkGenderAssignment(gen)
  
  #process steps
  gender <- genderAssignment(gen)
  mPhons[mPhons$phoneme == phon, c("canBeFirst", "canBeMid", "maleEnding", "femaleEnding")] <- c(fst, mid, gender["m"], gender["f"])
  commitPhons(mPhons)
  print(paste("Updated phoneme ", phon, " to Master Phonemes File"))
}

deletePhoneme <- function(phon) {
  # common validation
  mPhons <- getMasterPhonemes()
  
  newPhons <- mPhons[mPhons$phoneme != tolower(phon),]
  commitPhons(newPhons)
  print(paste("Removed phoneme ", phon, " from Master Phonemes File"))
}

deletePhonemes <- function(phons) sapply(phons, deletePhoneme)

addPhonemes <- function(phonDF) {
  if (!(is.data.frame(phonDF)) |
      (!(ncol(phonDF %>% c(5,13))))) {
    stop("You must provide a 5 or 13 column data frame")
  }
  mPhons <- getMasterPhonemes()
  checkFirstAndMid(phonDF[[2]], phonDF[[3]])
  sapply(as.character(phonDF$phoneme), checkExistsPhoneme, mPhons$phoneme)
  
  if (ncol(phonDF == 5)) {
    cbind(phonDF, rep(0,8))
  }
  
  newPhons <- rbind(mPhons, phonDF)
  commitPhons(newPhons)
  print("Added multiple phonemes to Master Phonemes File")
}

resetPhonRatings <- function(phon) {
  mPhons <- getMasterPhonemes()
  mPhons[mPhons$phoneme == "on", 6:13] <- 0
  commitPhons(mPhons)
  print(paste("Ratings reset to zero for phoneme", phon))
}
  
