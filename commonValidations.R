checkFirstAndLast <- function(fst, mid, lst) {
  if (!(is.logical(fst) & is.logical(mid) & is.logical(lst))) {
    stop("Only TRUE or FALSE (logical) values allowed for first and last placement")
  }
}

checkGenderAssignment <- function(gen) {
  for (g in gen) {
    if(!(any(g %in% c("m", "f", "u")))) {
      stop(paste("gender must be one of m, f or u"))
    }
  } 
}

checkExistsPhoneme <- function(phon, phons) {
  for (ph in phon) {
    if(any(phons == phon)) {
      stop(paste(ph, "already exists, use updatePhoneme to change existing"))
    }
  }
}