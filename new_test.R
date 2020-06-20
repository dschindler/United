require(unitedR)
require(plyr)
a <- formation(5,10, 10, 20, 10)
b <- formation(1,10, 10, 10, 25)
R <- unitedSim(a, b)
head(R@results)


chancesHome <- c(R@results$chancesAwayDF[1], R@results$chancesAwayMF[1], R@results$chancesAwayST[1])
chancesAway <- c(R@results$chancesAwayDF[1], R@results$chancesAwayMF[1], R@results$chancesAwayST[1])
probGoalHome <- R@results$probGoalHome[1]
probGoalAway <- R@results$probGoalAway[1]

chancesHome <- sum(floor(chancesHome / 3 + 0.5))
chancesAway <- sum(floor(chancesAway / 3 + 0.5))

overtime(chancesHome, chancesAway, probGoalHome, probGoalAway)


penaltyProbGoalHome <- 0.8
penaltyProbGoalAway <- 0.7
initial <- 5


# Possible results with Try
PR <- data.frame(goalsHome =    c(3, 3, 4, 4, 3), 
                 goalsAway =    c(0, 0, 1, 2, 0), 
                 attemptsHome = c(3, 3, 4, 4, 4),
                 attemptsAway = c(2, 3, 3, 4, 3))

allResultsTrunc <- merge(allResults, NP, all = TRUE)
allResultsTrunc <- allResultsTrunc[is.na(allResultsTrunc$remark), 1:2]


# compute probability of results
allResultsTrunc$probability <- apply(allResultsTrunc, 1, function(x) {
  probHome <- dbinom(x[1], chancesHome, prob = penaltyProbGoalHome)
  probAway <- dbinom(x[2], chancesAway, prob = penaltyProbGoalAway)
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




penaltyShootout <- function(penaltyProbGoalHome, penaltyProbGoalAway, initial = 5) {
  if(all(c(penaltyProbGoalHome, penaltyProbGoalAway) == 0)) {
    penaltyProbGoalHome <- penaltyProbGoalAway <- 0.5
  }
  return('hi')  
}


#Output: ProbDraw, ProbWin, ProbLoss
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








