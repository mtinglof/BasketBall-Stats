playercreate = function(name, cost, n, starting){
  library("readxl")
  playerexcel = read_excel("D:/Dev/PredBasketball/lebjam.xlsx", sheet = name) 
  
  player.fieldgoal = playerexcel$FG + 10
  player.rebounds = playerexcel$TRB + 10
  player.assists = playerexcel$AST + 10 
  player.turnover = playerexcel$TOV + 10 
  
  fieldgoal = matrix(rnorm(n, mean=mean(log(player.fieldgoal)), sd=sd(log(player.fieldgoal))))
  three.parameter = (dim(playerexcel)[1]-2)/sum(playerexcel$'3P')
  threepoints = matrix(rexp(n, three.parameter))
  rebounds = matrix(rnorm(n, mean=mean(log(player.rebounds)), sd=sd(log(player.rebounds))))
  total.assists = matrix(rnorm(n, mean=mean(log(player.assists)), sd=sd(log(player.assists))))
  steals.parameter = (dim(playerexcel)[1]-2)/sum(playerexcel$STL)
  steals = matrix(rexp(n, steals.parameter))
  blocks.parameter = (dim(playerexcel)[1]-2)/sum(playerexcel$BLK)
  blocks = matrix(rexp(n, blocks.parameter))
  turnover = matrix(rnorm(n, mean=mean(log(player.turnover)), sd=sd(log(player.turnover))))
  
  fieldgoal.percent = 1+(.5-pnorm(abs(fieldgoal-mean(log(player.fieldgoal)))/sd(log(player.fieldgoal))))*2
  threepoints.percent = exp(-three.parameter*abs(threepoints-(1/three.parameter)))
  rebounds.percent = 1+(.5-pnorm(abs(rebounds-mean(log(player.rebounds)))/sd(log(player.rebounds))))*2
  total.assists.percent = 1+(.5-pnorm(abs(total.assists-mean(log(player.assists)))/sd(log(player.assists))))*2
  steals.percent = exp(-steals.parameter*abs(steals-(1/steals.parameter)))
  blocks.percent = exp(-blocks.parameter*abs(blocks-(1/blocks.parameter)))
  turnover.percent = 1+(.5-pnorm(abs(turnover-mean(log(player.turnover)))/sd(log(player.turnover))))*2
  total.percent = fieldgoal.percent*threepoints.percent*rebounds.percent*total.assists.percent*steals.percent*blocks.percent*turnover
  
  weights = matrix(c(0, 1, 1.5, 1.25, 1.5, 2, 2, -.5, 0))
  player = cbind(cost, exp(fieldgoal)-10, threepoints, exp(rebounds)-10, exp(total.assists)-10, steals, blocks, exp(turnover)-10, total.percent)
  score = player%*%weights
  player =cbind(player, score)
  player.dist =cbind(cost, mean(player[,10]), sd(player[,10])) 
  player.dist = cbind(signif(player.dist, digits = 3), starting)
  
  #colnames(player) = c("cost", "2pt", "3pt", "rebounds", "assists", "steals", "blocks", "turnover", "T%", "points")
  colnames(player.dist) = c("cost", "mean", "sd", "starting")
  rownames(player.dist) = c(name)
  return(player.dist) 
}

teamcombos = function(){
  pg.data = read_excel("D:/Dev/PredBasketball/MILvSAC-NOvLAL.xlsx", sheet="PG")
  sg.data = read_excel("D:/Dev/PredBasketball/MILvSAC-NOvLAL.xlsx", sheet="SG")
  sf.data = read_excel("D:/Dev/PredBasketball/MILvSAC-NOvLAL.xlsx", sheet="SF")
  pf.data = read_excel("D:/Dev/PredBasketball/MILvSAC-NOvLAL.xlsx", sheet="PF")
  c.data = read_excel("D:/Dev/PredBasketball/MILvSAC-NOvLAL.xlsx", sheet="C")
  g.data = read_excel("D:/Dev/PredBasketball/MILvSAC-NOvLAL.xlsx", sheet="G")
  f.data = read_excel("D:/Dev/PredBasketball/MILvSAC-NOvLAL.xlsx", sheet="F")
  util.data = read_excel("D:/Dev/PredBasketball/MILvSAC-NOvLAL.xlsx", sheet="UTIL")
  
  
}