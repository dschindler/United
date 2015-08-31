#' @include formation.R
#' @include penaltyGoalsProb.R
#' @include unitedSimClass.R
NULL

###############################################
# --------------------------------------------#
# unitedSimOne                                #
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
#' home <- formation(10, NA, c(7,5,3), c(8,8), c(10,10,8))
#' away <- formation(5, 8, c(8,8), c(10,10), c(10,10,10), 
#'  hardness = c(0,0,0,0,1))
#' set.seed(123)
#' unitedSimOne(home, away)
#' 
#' @export
unitedSimOne <- function(home, away, r) {
  stopifnot(validObject(home), validObject(away), is(home, "formation"), 
            is(home, "formation"))
  if (missing(r)) {
    if (sum(home@hardness) > 1 || sum(away@hardness > 1)) {
      warning("You should simulate hardness and penalties, calculations are not exact.")
    }
    
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
  
    # possible penalties home
    posPenaltiesHome <- sum(away@hardness)
    posPenaltiesAway <- sum(home@hardness)
  
    # probability of a goal of a penalty
    penaltyProbGoalHome <- 1 - (awayLineupSim[1] * 0.05)
    penaltyProbGoalAway <- 1 - (homeLineupSim[1] * 0.05)
  
    # probability distribution of all possible goals by penalties for both teams
    goalsPenaltyDistrHome <- penaltyGoalsProb(posPenaltiesHome, penaltyProbGoalHome)
    goalsPenaltyDistrAway <- penaltyGoalsProb(posPenaltiesAway, penaltyProbGoalAway)
  
    # possible allocations of the penalties
    penaltyAllocations <- expand.grid(home = 0:posPenaltiesHome, away = 0:posPenaltiesAway)
    penaltyAllocations$probability <- apply(penaltyAllocations, 1, function(x) {
        probHome <- goalsPenaltyDistrHome$probability[which(goalsPenaltyDistrHome$goals == x[1])]
        probAway <- goalsPenaltyDistrAway$probability[which(goalsPenaltyDistrAway$goals == x[2])]
        probHome * probAway
      }
    )
  
    # compute all possible results
    allResults <- expand.grid(goalsHome = 0:chancesHome, goalsAway = 0:chancesAway)
    # compute probability of a goal
    probGoalAway <- (1-homeLineupSim[1]/15) * (1-homeLineupSim[2]/14)
    probGoalHome <- (1-awayLineupSim[1]/15) * (1-awayLineupSim[2]/14)
    # compute probability of results
    allResults$probability <- apply(allResults, 1, function(x) {
        probHome <- dbinom(x[1], chancesHome, prob = probGoalHome)
        probAway <- dbinom(x[2], chancesAway, prob = probGoalAway)
        probHome * probAway
      }
    )
  
    # list for all possible results including penalties
    allResWithPen <- vector("list", nrow(penaltyAllocations))
    for (i in 1:nrow(penaltyAllocations)) {
      resultsPenalty <- allResults
      resultsPenalty$goalsHome <- resultsPenalty$goalsHome + penaltyAllocations$home[i]
      resultsPenalty$goalsAway <- resultsPenalty$goalsAway + penaltyAllocations$away[i]
      resultsPenalty$probability <- resultsPenalty$probability * penaltyAllocations$probability[i]
      allResWithPen[[i]] <- resultsPenalty
    }
  
    # merge the data.frames
    finalPossibleResults <- do.call("rbind", allResWithPen)
  
    finalPossibleResults <- ddply(finalPossibleResults, .(goalsHome, goalsAway), summarize, 
                                probability = sum(probability))
  
  
    # sort results by probability of apprearance
    finalPossibleResults <- finalPossibleResults[order(finalPossibleResults$probability, decreasing = TRUE), ]
    # add a cumsum of the probabilities
    finalPossibleResults$cumsumProb <- cumsum(finalPossibleResults$probability)
    # add points for home
    finalPossibleResults$pointsHome <- ifelse(finalPossibleResults$goalsHome > finalPossibleResults$goalsAway, 3, 
                                            ifelse(finalPossibleResults$goalsHome == finalPossibleResults$goalsAway, 1,
                                                   0))
    # add points for away
    finalPossibleResults$pointsAway <- ifelse(finalPossibleResults$goalsHome < finalPossibleResults$goalsAway, 3, 
                                            ifelse(finalPossibleResults$goalsHome == finalPossibleResults$goalsAway, 1, 
                                                   0))
    # add training points (TP) for home
    finalPossibleResults$tpHome <- ifelse(finalPossibleResults$goalsHome > finalPossibleResults$goalsAway, 1, 
                                        ifelse(finalPossibleResults$goalsHome == finalPossibleResults$goalsAway, 0.5, 
                                               0))
    # add traings points (TP) for away
    finalPossibleResults$tpAway <- ifelse(finalPossibleResults$goalsHome < finalPossibleResults$goalsAway, 1, 
                                        ifelse(finalPossibleResults$goalsHome == finalPossibleResults$goalsAway, 0.5, 
                                               0))
    # output
    output <- new("unitedSim", 
                results = finalPossibleResults, 
                averageTrainingPointsHome = round(sum(finalPossibleResults$tpHome * finalPossibleResults$probability), digits = 4),
                averageTrainingPointsAway = round(sum(finalPossibleResults$tpAway * finalPossibleResults$probability), digits = 4), 
                averagePointsHome = round(sum(finalPossibleResults$pointsHome * finalPossibleResults$probability), digits = 4),
                averagePointsAway = round(sum(finalPossibleResults$pointsAway * finalPossibleResults$probability), digits = 4),
                winProbabilityHome = round(sum((finalPossibleResults$pointsHome == 3) * finalPossibleResults$probability), digits = 4),
                winProbabilityAway = round(sum((finalPossibleResults$pointsAway == 3) * finalPossibleResults$probability), digits = 4), 
                tiedProbability = round(sum((finalPossibleResults$pointsAway == 1) * finalPossibleResults$probability), digits = 4),
                home = home,
                away = away)
  
    return(output)
    # include red Cards when simulating
  } else {
    stopifnot(is.numeric(r), round(r) == r, length(r) == 1)
    sapply(1:r,function(x) { 
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
              
              # possible penalties home
              posPenaltiesHome <- sum(away@hardness)
              posPenaltiesAway <- sum(home@hardness)
              
              # probability of a goal of a penalty
              penaltyProbGoalHome <- 1 - (awayLineupSim[1] * 0.05)
              penaltyProbGoalAway <- 1 - (homeLineupSim[1] * 0.05)
              
              return(awayLineupSim)
      }
    ) 
  
}
