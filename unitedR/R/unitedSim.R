#' @include unitedSimOne.R
#' @include unitedSimResults.R
NULL

###############################################
# --------------------------------------------#
# unitedSim                                   #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Main Function for simulation line ups
# --------------------------------------------

#' Simulating a formation
#' 
#' Simulates a formation against another formations (several formations of away are possible).
#' 
#' @inheritParams overview
#' @param ... several objects of the class \code{formation}
#'
#' @return Creates an object of the \code{unitedSim} class.
#' 
#' @examples 
#' home <- formation(10, NA, c(7,5,3), c(8,8), c(10,10,8))
#' away <- formation(5, 8, c(8,8), c(10,10), c(10,10,10), 
#'  hardness = c(0,0,0,0,1))
#' set.seed(123)
#' unitedSim(home, away)
#' # can also be simualated
#' unitedSim(home, away, r = 100)
#' # several away lineups
#' unitedSim(home, away, away)
#' # several away lineups simulated
#' unitedSim(home, away, away, r = 100)
#' # used hardness matrix (default)
#' hardnessMatrix <- matrix(c(90,10,0,0,0,0,0,0,
#' 70,30,0,0,0,0,0,0,50,40,10,
#' 0,0,0,0,0,30,50,20,0,0,0,0,0,20,40,30,10,0,0,
#' 10,30,40,20,0,0,0,0,0,20,40,30,10,0,0,0,0,10,20,
#' 40,20,10,0,0,0,0,10,40,20,20,10), nrow = 8)
#' 
#' @export
unitedSim <- function(home, ..., r, penaltyProb = 0.1, preventGoalGK = 1/14, preventGoalSW = 1/15, hardnessMatrix) {
  stopifnot(validObject(home), is(home, "formation"))
  
  ## set default value for hardness matrix
  if (missing(hardnessMatrix)) {
    hardnessMatrix <- matrix(c(90,10,0,0,0,0,0,0,70,30,0,0,0,0,0,0,50,40,10,
                               0,0,0,0,0,30,50,20,0,0,0,0,0,20,40,30,10,0,0,
                               0,0, 10,30,40,20,0,0,0,0,0,20,40,30,10,0,0,0,0,
                               10,30,40,20,0,0,0,0,0,20,40,30,10,0,0,0,0,10,20,
                               40,20,10,0,0,0,0,10,40,20,20,10), nrow = 8)
  }
  
  formations <- list(...)
  if (!all(sapply(formations, function(x)  is(x, "formation"))))
    stop("Not all ... objects of class formation.")
  if (missing(r)) {
    if (length(formations) == 1) {
      return(unitedSimOne(home, formations[[1]], penaltyProb = penaltyProb, preventGoalGK = preventGoalGK, 
                          preventGoalSW = preventGoalSW, hardnessMatrix = hardnessMatrix))
    } else {
      games <- lapply(formations, function(formation) {
          unitedSimOne(home, formation, penaltyProb = penaltyProb, preventGoalGK = preventGoalGK, 
                       preventGoalSW = preventGoalSW, hardnessMatrix = hardnessMatrix)  
        }
      )
    }
    return(new("unitedSimResults", games = games))
  } else {
    stopifnot(is.numeric(r), round(r) == r, length(r) == 1)
      if (length(formations) == 1) {
        return(unitedSimOne(home, formations[[1]], r = r, penaltyProb = penaltyProb, preventGoalGK = preventGoalGK,
                            preventGoalSW = preventGoalSW, hardnessMatrix = hardnessMatrix))
      } else {
        games <- lapply(formations, function(formation) {
          unitedSimOne(home, formation, r = r, penaltyProb = penaltyProb, preventGoalGK = preventGoalGK, 
                       preventGoalSW = preventGoalSW, hardnessMatrix = hardnessMatrix)  
        }
      )
    }
    return(new("unitedSimResults", games = games))
  }  
}
