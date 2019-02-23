playercreate(name, path, cost){
  playerexcel = read_excel(path, sheet = name) 
  fg = rnorm(1, mean=mean(log(playerexcel$FG)), sd=sd(log(playerexcel$FG)))
  para3 = (dim(playerexcel)[1]-2)/sum(playerexcel$'3P')
  p3 = rexp(1, para3)
  
  fgp = 1+(.5-pnorm(abs(fg-mean(log(playerexcel$FG)))/sd(log(playerexcel$FG))))*2
  p3p = exp(-para3*abs(p3-(1/para3)))
  
  player = c(fg, p3)
  rowname(player) = name
  colnames(player) = c("cost", "2pt", "3pt", "rebounds", "assists", "steals", "blocks", "%")
  return(player)
}