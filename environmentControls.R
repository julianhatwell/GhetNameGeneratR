ghetInit <- function() {
  source("environmentControls.R")
  source("commonValidations.R")
  source("commonActions.R")
  source("generateName.R")
  assign("GhetNameEnv", new.env(), envir=.GlobalEnv)
  
  if (is.null(get0("MasterPhonemes", envir=GhetNameEnv))) {
  
  if (!(file.exists("MasterPhonemes.txt"))) {
    stop("Missing Master Phonemes File")
  }
  
  assign("MasterPhonemes", envir = GhetNameEnv
         , read.csv("MasterPhonemes.txt"
                    , header = TRUE
                    , stringsAsFactors = FALSE))

  # TO DO - get performance/probablities
  
  }
}

ghetClose <- function() {
  # NB Right now concurrency = 1
  if (nrow(GhetNameEnv$MasterPhonemes) > 0) {
  write.csv(GhetNameEnv$MasterPhonemes, "MasterPhonemes.txt", row.names = FALSE)
  }
  rm(GhetNameEnv, envir = .GlobalEnv)
  
  # TO DO - keep all the functions in the environment and get rid of everything on exit
}