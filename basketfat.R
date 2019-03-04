playercreate = function(name, n){
  library("readxl")
  path = "D:/Dev/PredBasketball/BasketBall-Stats/PlayerExport.xlsx"
  playerexcel = read_excel(path=path, sheet = name) 
  
  player.freethrow = playerexcel$FT + 10
  player.fieldgoal = playerexcel$FG + 10
  player.rebounds = playerexcel$TRB + 10
  player.assists = playerexcel$AST + 10 
  player.turnover = playerexcel$TOV + 10
  
  freethrow = matrix(rnorm(n, mean=mean(log(player.freethrow)), sd=sd(log(player.freethrow))))
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
  
  freethrow.percent = 1+(.5-pnorm(abs(freethrow-mean(log(player.freethrow)))/sd(log(player.freethrow))))*2
  fieldgoal.percent = 1+(.5-pnorm(abs(fieldgoal-mean(log(player.fieldgoal)))/sd(log(player.fieldgoal))))*2
  threepoints.percent = exp(-three.parameter*abs(threepoints-(1/three.parameter)))
  rebounds.percent = 1+(.5-pnorm(abs(rebounds-mean(log(player.rebounds)))/sd(log(player.rebounds))))*2
  total.assists.percent = 1+(.5-pnorm(abs(total.assists-mean(log(player.assists)))/sd(log(player.assists))))*2
  steals.percent = exp(-steals.parameter*abs(steals-(1/steals.parameter)))
  blocks.percent = exp(-blocks.parameter*abs(blocks-(1/blocks.parameter)))
  turnover.percent = 1+(.5-pnorm(abs(turnover-mean(log(player.turnover)))/sd(log(player.turnover))))*2
  total.percent = fieldgoal.percent*threepoints.percent*rebounds.percent*total.assists.percent*steals.percent*blocks.percent*turnover
  
  weights = matrix(c(1, 2, 3.5, 1.25, 1.5, 2, 2, -.5))
  player = cbind(exp(freethrow)-10, exp(fieldgoal)-10, threepoints, exp(rebounds)-10, exp(total.assists)-10, steals, blocks, exp(turnover)-10)
  score = player%*%weights
  player =cbind(player, score)
  #player = cbind(player, total.percent)
  player.dist =cbind(mean(player[,9]), sd(player[,9])) 
  player.dist = (signif(player.dist, digits = 3))
  
  #colnames(player) = c("FT", "2pt", "3pt", "rebounds", "assists", "steals", "blocks", "turnover", "points", "T%")
  colnames(player.dist) = c("mean", "sd")
  rownames(player.dist) = c(name)
  return(player.dist) 
}

playergen = function(n){
  library("readxl")
  path = "D:/Dev/PredBasketball/BasketBall-Stats/MILvSAC-NOvLAL.xlsx"
  all.data = read_excel(path=path, sheet="ALL")
  all.player = matrix(data=NA, nrow = dim(all.data)[1], ncol = 2)
  i = 1
  for(name in all.data$Name){
    player = playercreate(name, n)
    all.player[i, 1] = player[1, 1]
    all.player[i, 2] = player[1, 2]
    i = i + 1
  }
  #rownames(all.player) = all.data$Name
  colnames(all.player) = c("Mean", "SD")
  return(all.player)
}

playercheck = function(name){
  library("readxl")
  path = "D:/Dev/PredBasketball/BasketBall-Stats/PlayerExport.xlsx"
  playerexcel = read_excel(path=path, sheet = name) 
  weights = matrix(c(1, 2, 3.5, 1.25, 1.5, 2, 2, -.5))
  player = cbind(playerexcel$FT, playerexcel$FG, playerexcel$'3P', playerexcel$TRB, playerexcel$AST, playerexcel$STL, playerexcel$BLK, playerexcel$TOV)
  playerpoints = player%*%weights
  return(c(mean(playerpoints), sd(playerpoints)))
}

playercombos = function(n, gensize){
  library("readxl")
  path = "D:/Dev/PredBasketball/BasketBall-Stats/MILvSAC-NOvLAL.xlsx"
  all.data = read_excel(path=path, sheet="ALL")
  
  pg.index = which(all.data$Position %in% c("PG", "PG/SG", "PG/SF"))
  sg.index = which(all.data$Position %in% c("PG/SG", "SG/SF"))
  sf.index = which(all.data$Position %in% c("PG/SF", "SF/PF", "SG/SF"))
  pf.index = which(all.data$Position %in% c("PF", "PF/C", "SF/PF"))
  c.index = which(all.data$Position %in% c("C", "PF/C"))
  g.index = which(all.data$Position %in% c("PG", "PG/SG", "PG/SF", "SG/SF"))
  f.index = which(all.data$Position %in% c("PG/SF", "SG/SF", "PF", "PF/C", "SF/PF"))
  util.index = 1:dim(all.data)[1]
  
  generated.data = playergen(gensize)
  all.data = cbind(all.data, generated.data)
  
  highscore = 0
  finalsheet = 0
  j = 0 
  index = (unique(c(sample(pg.index, n, replace = T), sample(sg.index, n, replace = T), sample(sf.index, n, replace = T), sample(pf.index, n, replace = T), sample(c.index, n, replace = T), 
    sample(g.index, n, replace = T), sample(f.index, n, replace = T), sample(util.index, n, replace = T))))
  index = (matrix(index, nrow = n, ncol = 8))
  for(i in 1:n){
    combopoints = sum(all.data$Mean[index[i,]]*(all.data$SD[index[i,]]*2))
    combocost = sum(all.data$Salary[index[i,]])
    if((combopoints > highscore) & (combocost < 50001)){
      finalteam = t(all.data$Name[index[i,]])
      finalteam = cbind(finalteam, combopoints)
      finalsheet[j] = finalteam
      j = j + 1
    }
  }
  return(finalsheet)
}
