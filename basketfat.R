playercreate(name, path, cost, n){
  playerexcel = read_excel(path, sheet = name) 
  fg = matrix(rnorm(n, mean=mean(log(playerexcel$FG)), sd=sd(log(playerexcel$FG))))
  para3 = (dim(playerexcel)[1]-2)/sum(playerexcel$'3P')
  p3 = matrix(rexp(n, para3))
  rb = matrix(rnorm(n, mean=mean(log(playerexcel$TRB)), sd=sd(log(playerexcel$TRB))))
  ta = matrix(rnorm(n, mean=mean(log(playerexcel$FG)), sd=sd(log(playerexcel$FG))))
  stpara = (dim(playerexcel)[1]-2)/sum(playerexcel$STL)
  st = matrix(rexp(n, stpara))
  blpara = (dim(playerexcel)[1]-2)/sum(playerexcel$BLK)
  bl = matrix(rexp(n, blpara))
  
  fgp = 1+(.5-pnorm(abs(fg-mean(log(playerexcel$FG)))/sd(log(playerexcel$FG))))*2
  p3p = exp(-para3*abs(p3-(1/para3)))
  rbp = 1+(.5-pnorm(abs(rb-mean(log(playerexcel$TRB)))/sd(log(playerexcel$TRB))))*2
  #tap
  stp = exp(-stpara*abs(st-(1/stpara)))
  blp = exp(-blpara*abs(bl-(1/blpara)))
  total.percent = fgp*p3p*stp*blp
  
  player = cbind(fg, p3, rb, ta, st, bl, total.percent)
  rowname(player) = name
  colnames(player) = c("cost", "2pt", "3pt", "rebounds", "assists", "steals", "blocks", "TO", "%")
  return(player)
}