## Mafia

oneGame=function(total=total,bad=bad) {
  # Simulating 1 Mafia game.
  # Number of good people = total - bad
  # Number of bad people = bad
  # Kill players until only good or bad players remain
  # Return 1 if bad guys win, 0 if good guys win
  game=c(rep(0,total-bad),rep(1,bad))
  
  while(0 %in% game & 1 %in% game){
    x= sample(1:length(game),1)
    game=game[-x]
    #    print(game)
    if(0 %in% game & 1 %in% game) {
      x=sample(1:sum(game==0),1)
      game=game[-x]
      #      print(game)
    }
  }
  if (0 %in% game) {
    return(0)
  } else return(1)
}

mafia=function(total, bad, times) {
  # Simulate multiple games of Mafia
  # Giving number of times of simulations,
  # keep a running average of how many times
  # bad guys win.
  winner=0
  for(i in 1:times) {
    if(oneGame(total=total,bad=bad)==0) {
      winner=winner+1
    }
    print(winner/i)
  }
}

