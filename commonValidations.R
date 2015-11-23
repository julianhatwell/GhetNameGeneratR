checkExistsMaster <- function() {
  if (!(file.exists("MasterPhonemes.txt"))) {
    stop("Missing Master Phonemes File")
  }
  read.csv("MasterPhonemes.txt", header = TRUE, stringsAsFactors = FALSE)
}

checkFirstAndLast <- function(fst, lst) {
  if (!(is.logical(fst) | is.logical(lst))) {
    stop("Only TRUE or FALSE values allowed for first and last placement")
  }
}

checkExistsPhoneme <- function(phon, phons) {
  for (ph in phon) {
    if(any(phons == phon)) {
      stop(paste(ph, "already exists, use updatePhoneme to change existing"))
    }
  }
}