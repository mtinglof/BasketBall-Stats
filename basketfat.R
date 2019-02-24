playercreate(name, path, cost){
  playerexcel = read_excel(path, sheet = name) 
  fg = rnorm(1, mean=mean(log(playerexcel$FG)), sd=sd(log(playerexcel$FG)))
  para3 = (dim(playerexcel)[1]-2)/sum(playerexcel$'3P')
  p3 = rexp(1, para3)
  #rb = rnorm(1, mean=mean(playerexcel$TRB), sd=sd(playerexcel$TRB))
  #ta = rnorm(1, mean=mean(log(playerexcel$FG)), sd=sd(log(playerexcel$FG)))
  stpara = (dim(playerexcel)[1]-2)/sum(playerexcel$STL)
  st = rexp(1, stpara)
  blpara = (dim(playerexcel)[1]-2)/sum(playerexcel$BLK)
  bl = rexp(1, blpara)
  
  fgp = 1+(.5-pnorm(abs(fg-mean(log(playerexcel$FG)))/sd(log(playerexcel$FG))))*2
  p3p = exp(-para3*abs(p3-(1/para3)))
  #rbp = 1+(.5-pnorm(abs(rb-mean(playerexcel$TRB)))/sd(playerexcel$TRB))*2
  #tap
  stp = exp(-stpara*abs(st-(1/stpara)))
  blp = exp(-blpara*abs(bl-(1/blpara)))
  total.percent = fgp*p3p*stp*blp
  
  player = c(fg, p3, rb, ta, st, bl, total.percent)
  rowname(player) = name
  colnames(player) = c("cost", "2pt", "3pt", "rebounds", "assists", "steals", "blocks", "TO", "%")
  return(player)
}