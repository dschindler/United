#' @include formation.R
NULL

###############################################
# --------------------------------------------#
# unitedSim                                    #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Main Function for simulation line ups
# --------------------------------------------

#' Simulating a formation
#' 
#' Simulates a formation against another formation.
#' 
#' @inheritParams overview
#'
#' @return Creates an object of the \code{unitedSim} class.
#' 
#' @examples 
#' Home <- formation(10, NA, c(7,5,3), c(8,8), c(10,10,8))
#' Away <- formation(5, 8, c(8,8), c(10,10), c(10,10,10), 
#'  hardness = c(0,0,0,0,10))
#' set.seed(123)
#' unitedSim(Home, Away)
#' 
#'
#' @export
unitedSim <- function(home, away, r) {
  stopifnot(validObject(home), validObject(away), is(home, "formation"), 
            is(home, "formation"))
  if (missing(r)) {
    r <- 1
  }
  stopifnot(is.numeric(r), round(r) == r, length(r) == 1)
  homeLineup <- getLineup(home)
  awayLineup <- getLineup(away)
  
  # simulate red cards
  homeLineupSim <- simRedCard(home, homeLineup)
  awayLineupSim <- simRedCard(away, awayLineup)
  
  chancesHome <- round((homeLineupSim[3:5] - awayLineupSim[5:3] - 
                          c(0, 0, awayLineupSim[2])) * c(1/4, 1/2, 1))
  chancesHome <- sum(chancesHome[chancesHome > 0])
  
  chancesAway <- round((awayLineupSim[3:5] - homeLineupSim[5:3] - 
                          c(0, 0, homeLineupSim[2])) * c(1/4, 1/2, 1))
  chancesAway <- sum(chancesAway[chancesAway > 0])
  
  
  
  list(chancesAway = chancesAway, chancesHome = chancesHome)
  
  # simulate penalties
  
}
