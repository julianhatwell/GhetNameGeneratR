commitPhons <- function(phonDF) {
  write.csv(phonDF[order(phonDF$phoneme),], "MasterPhonemes.txt", row.names = FALSE)
}

makePhoneme <- function(phon, fst, lst) {
  # common validation
  mPhons <- checkExistsMaster()
  checkFirstAndLast(fst, lst)
  checkExistsPhoneme(phon, mPhons$phoneme)
  
  newPhons <- rbind(mPhons, c(tolower(phon), fst, lst))
  commitPhons(newPhons)
  print(paste("Added phoneme ", phon, " to Master Phonemes File"))
}

updatePhoneme <- function(phon, fst, lst) {
  # common validation
  mPhons <- checkExistsMaster()
  checkFirstAndLast(fst, lst)
  
  phon <- tolower(phon)
  newPhons <- rbind(mPhons[mPhons$phoneme != phon,], c(phon, fst, lst))
  commitPhons(newPhons)
  print(paste("Updated phoneme ", phon, " to Master Phonemes File"))
}

deletePhoneme <- function(phon) {
  # common validation
  mPhons <- checkExistsMaster()
  
  newPhons <- mPhons[mPhons$phoneme != tolower(phon),]
  commitPhons(newPhons)
  print(paste("Removed phoneme ", phon, " from Master Phonemes File"))
}

deletePhonemes <- function(phons) sapply(phons, deletePhoneme)

permutePhoneme <- function(stem) {
  adjuncts <- c("'", "a", "e", "i", "o", "u", "y")
  phoneme <- character(0)
  for (adj in adjuncts) {
    phoneme <- c(phoneme, paste0(stem, adj))
  }
  canBeFirst <- rep(TRUE, times = length(adjuncts))
  canBeLast <- c(FALSE, rep(TRUE, times = length(adjuncts) - 1))
  data.frame(phoneme, canBeFirst, canBeLast)
}

addPhonemes <- function(phonDF) {
  if (!(is.data.frame(phonDF)) |
      (!(ncol(phonDF == 3)))) {
    stop("You must provide a three column data frame")
  }
  mPhons <- checkExistsMaster()
  checkFirstAndLast(phonDF[[2]], phonDF[[3]])
  sapply(as.character(phonDF$phoneme), checkExistsPhoneme, mPhons$phoneme)
  
  newPhons <- rbind(mPhons, phonDF)
  commitPhons(newPhons)
  print("Added multiple phonemes to Master Phonemes File")
}
