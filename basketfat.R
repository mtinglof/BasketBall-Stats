playercreate = function(name, cost, n){
  library("readxl")
  playerexcel = read_excel("D:/Dev/PredBasketball/lebjam", sheet = name) 
  fg = matrix(rnorm(n, mean=mean(log(playerexcel$FG)), sd=sd(log(playerexcel$FG))))
  para3 = (dim(playerexcel)[1]-2)/sum(playerexcel$'3P')
  p3 = matrix(rexp(n, para3))
  rb = matrix(rnorm(n, mean=mean(log(playerexcel$TRB)), sd=sd(log(playerexcel$TRB))))
  ta = matrix(rnorm(n, mean=mean(log(playerexcel$AST)), sd=sd(log(playerexcel$AST))))
  stpara = (dim(playerexcel)[1]-2)/sum(playerexcel$STL)
  st = matrix(rexp(n, stpara))
  blpara = (dim(playerexcel)[1]-2)/sum(playerexcel$BLK)
  bl = matrix(rexp(n, blpara))
  to = matrix(rnorm(n, mean=mean(log(playerexcel$TOV)), sd=sd(log(playerexcel$TOV))))
  
  fgp = 1+(.5-pnorm(abs(fg-mean(log(playerexcel$FG)))/sd(log(playerexcel$FG))))*2
  p3p = exp(-para3*abs(p3-(1/para3)))
  rbp = 1+(.5-pnorm(abs(rb-mean(log(playerexcel$TRB)))/sd(log(playerexcel$TRB))))*2
  tap = 1+(.5-pnorm(abs(ta-mean(log(playerexcel$AST)))/sd(log(playerexcel$AST))))*2
  stp = exp(-stpara*abs(st-(1/stpara)))
  blp = exp(-blpara*abs(bl-(1/blpara)))
  to = 1+(.5-pnorm(abs(to-mean(log(playerexcel$TOV)))/sd(log(playerexcel$TOV))))*2
  total.percent = fgp*p3p*rbp*tap*stp*blp*to
  
  player = cbind(fg, p3, rb, ta, st, bl, to, total.percent)
  rowname(player) = name
  colnames(player) = c("cost", "2pt", "3pt", "rebounds", "assists", "steals", "blocks", "TO", "%")
  return(player)
}

