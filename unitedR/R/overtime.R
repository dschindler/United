###############################################
# --------------------------------------------#
# overtime                                    #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Main Function for computing overtime results
# --------------------------------------------

#' Computing overtime results
#' 
#' Computes the final overtime outcome.
#' 
#' @inheritParams overview
#'
#' @return \code{list} with probabilities of final outcome.
#' 
#' 
#' @export
overtime <- function(chancesHome, chancesAway, probGoalHome, probGoalAway) {
  
  # compute all possible results
  allResults <- expand.grid(goalsHome = 0:chancesHome, goalsAway = 0:chancesAway)
  # compute probability of results
  allResults$probability <- apply(allResults, 1, function(x) {
    probHome <- dbinom(x[1], chancesHome, prob = probGoalHome)
    probAway <- dbinom(x[2], chancesAway, prob = probGoalAway)
    probHome * probAway
  }
  )
  
  allResults$result <- ifelse(allResults$goalsHome == allResults$goalsAway, "tiedProbability", 
                              ifelse(allResults$goalsHome > allResults$goalsAway, 'winProbabilityHome',
                                     'winProbabilityAway'))

  out <- ddply(allResults, .(result), summarize, probability = sum(probability))
  
  list('tiedProbability' = max(c(out[out[,1] == 'tiedProbability', 2]), 0),
       'winProbabilityHome' = max(c(out[out[,1] == 'winProbabilityHome', 2]), 0),
       'winProbabilityAway' = max(c(out[out[,1] == 'winProbabilityAway', 2]), 0)
  )
}

  
  