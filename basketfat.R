playercreate(name, path, cost){
  playerexcel = read_excel(path, sheet = name) 
  fg = rnorm(1, mean=mean(log(playerexcel$FG)), sd=sd(log(playerexcel$FG))))
  player = matrix(cost, fg, 
  rowname(player) = name
  colnames(player) = c("cost", "2pt", "%", "3pt", "%", "rebounds", "%", "assists", "%", "steals", "%", "blocks", "%")
  return(player)
}