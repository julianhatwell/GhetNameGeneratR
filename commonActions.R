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
  newPhons <- rbind(mPhons, c(tolower(phon), fst, mid, gender["m"], gender["f"]))
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
  phon <- tolower(phon)
  newPhons <- rbind(mPhons[mPhons$phoneme != phon,], c(phon, fst, mid, gender["m"], gender["f"]))
  commitPhons(newPhons)
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
      (!(ncol(phonDF == 5)))) {
    stop("You must provide a 5 column data frame")
  }
  mPhons <- getMasterPhonemes()
  checkFirstAndMid(phonDF[[2]], phonDF[[3]])
  sapply(as.character(phonDF$phoneme), checkExistsPhoneme, mPhons$phoneme)
  
  newPhons <- rbind(mPhons, phonDF)
  commitPhons(newPhons)
  print("Added multiple phonemes to Master Phonemes File")
}
