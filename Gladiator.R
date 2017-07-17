## Gladiator Statistical game
### Given 2 battling factions, each of them have an determined number of 
### warriors. Each warrior has a certain Hit Points. Each battle 
### consists of 1 vs 1 battle between a warrior of each faction. The
### probability of either warrior winning is the probability of 
### his Hit Points over the sum of both warriors' Hit Points.
### The winning warrior returns to his faction unscathed and can be 
### readily available for battle. The losing warrior is vanquished 
### and destroyed. The end of the game is when only one faction 
### remains and the other faction is obliterated. What is the optimal 
### strategy if it is known each faction's warrior's Hit Points?
### Hint: Probability of faction winning can be modeled as exponential
### distribution.


generatingFighters=function(power) {
  players=sample(2:power,1)
  individuals=numeric()
  for(i in 1:(players-1)) {
    individuals[i]=sample(0:power,1)
    power=power-individuals[i]
#    print(power)
  }
  individuals[players]=power
  individuals=individuals[!(individuals==0)]
  return(individuals)
}



singleMatch=function(power1=power1,power2=power2) {
  individuals1=generatingFighters(power1)
  individuals2=generatingFighters(power2)
  #print(individuals1)
  #print(individuals2)
  
  while(length(individuals1)!=0 &length(individuals2)!=0) {
    fighter1=sample(1:length(individuals1),1)
    fighter2=sample(1:length(individuals2),1)
    if(runif(1, min=0, max=(individuals1[fighter1] + individuals2[fighter2]) ) > 
         individuals1[fighter1] ) {
      individuals2[fighter2]=individuals1[fighter1] + individuals2[fighter2]
      individuals1=individuals1[-fighter1]
    } else {
      individuals1[fighter1]=individuals1[fighter1] + individuals2[fighter2]
      individuals2=individuals2[-fighter2]
    }
    
  }
  if(length(individuals1)!=0) return(0) else return(1)
}
  

gladiator=function(times) {
  power1=sample(1:100,1)
  power2=sample(1:100,1)
  counter=numeric()
  for (i in 1:times) {
    counter[i]=singleMatch(power1,power2)
    #print(counter)
  }
  print(c("Team One's Power",power1, "Team Two's Power", power2))
  print(c("Probabilty that Team One Wins", power1/(power1+power2),
          "Actual Result", sum(counter==0)/i) )
}