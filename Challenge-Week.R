play.shreddr = function() {
  ### setup
  pixels = matrix(c(
    # 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#0
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
    1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
    1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 1, 1, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,#4
    1, 1, 1, 1, 1, 1, 1, 4, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1,#5
    1, 1, 1, 1, 1, 1, 4, 4, 4, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1,#6
    1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1,#7
    1, 1, 1, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1,#8
    1, 1, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,#9
    1, 1, 1, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1,#10
    1, 1, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#11
    1, 1, 1, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#12
    1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#13
    5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#14
    5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#15
    5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#15
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#14
    1, 1, 1, 6, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#13
    1, 6, 6, 6, 6, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#12
    1, 1, 6, 6, 6, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#11
    1, 1, 1, 6, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 11, 11, 1, 1, 1, 1, 1,#10
    1, 1, 1, 6, 1, 1, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 11, 11, 11, 11, 1, 1, 1, 1,#9
    1, 1, 1, 1, 1, 1, 7, 7, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 11, 11, 11, 11, 11, 11, 1, 1, 1,#8
    1, 1, 1, 1, 1, 7, 7, 7, 7, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 10, 1, 1, 1, 11, 11, 11, 11, 1, 1, 1, 1,#7
    1, 1, 1, 1, 1, 1, 7, 7, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 10, 10, 10, 1, 1, 1, 11, 11, 1, 1, 1, 1, 1,#6
    1, 1, 1, 1, 1, 1, 1, 7, 1, 1, 1, 8, 8, 1, 1, 1, 1, 1, 10, 10, 10, 10, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 8, 8, 8, 8, 8, 1, 1, 1, 1, 10, 10, 10, 10, 10, 1, 1, 1, 1, 1, 1, 1, 1,#4
    1, 1, 1, 1, 1, 1, 1, 1, 1, 8, 8, 8, 8, 8, 1, 1, 9, 9, 1, 1, 10, 10, 10, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 8, 8, 1, 1, 9, 9, 9, 9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 9, 9, 9, 9, 9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 9, 9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1#0
  ), nrow = 32, byrow = TRUE)
  capcolors = c('#ffffff', '#1f75b7', '#156ba7', '#145b8f', '#0a4f7e', '#0e4368', '#093452', '#04273b', '#0e1c29', '#11161a', '#010101')
  
  play.game = function() {
    play = readline("Are you READY to play the AWESOME-EST Shreddr game in EXISTENCE? y/n ")
    ifelse(play =='y', print("Hurray, knew it. You're cool!"), print("Well, sorry to be the bearer of bad news but you're STUCK with me forever...until the next round!"))
    image(t(apply(pixels, 2, rev)), col = capcolors, axes=FALSE, main="Let's play a game...")
  }
  
  direction.question = function() {
    the.answer = readline("Mr/Miss Fruit...Cap Ninja, would you like to slice vertically or horizontally? v/h/quit: ")
    if(the.answer == "quit") return(the.answer)
    while(the.answer != 'v' & the.answer != 'h' & the.answer != 'quit') {
      the.answer = readline("I SAID Mr/Miss Captricity Ninja, would you like to slice vertically or horizontally? v/h/quit: ")
    }
    the.index = as.numeric(readline("Well, super-slicing-dicing-Magic-Bullet/Ninja, select your favorite number from 3 to 30: "))
    while(the.index < 3 | 30 < the.index ){
      the.index = as.numeric(readline("Must be a $5 handmixer... PLEASE select your favorite number from 3 to 30: "))
    }
    return(c(the.answer, as.character(the.index)))
  }
  
  quitting.time = function() {
    the.name = readline("All right! We gotta save YOUR Captricity logo. What's your name: ")
    the.name = gsub(" " ,"", the.name)
    print(paste("No, I'm mean a REAL name... JK. Here you go, ", the.name, "!"))
    jpeg(paste(as.character(the.name), "-loves-Captricity.jpg", sep = ''))
    image(t(apply(pixels, 2, rev)), col = capcolors, axes=FALSE, main="Let's play a game...")
    image(t(apply(pixels, 2, rev)), col = capcolors, axes=FALSE, main=paste(as.character(the.name), "-loves-Captricity.jpg", sep = ''))
    mtext(text=1:32, side=1, line=0.5, at=seq(0,1, length.out = 32), las=1, cex=1)
    mtext(text=32:1, side=2, line=0.5, at=seq(0,1, length.out = 32), las=1, cex=1)
    mtext(text=1:32, side=3, line=0.5, at=seq(0,1, length.out = 32), las=1, cex=1)
    mtext(text=32:1, side=4, line=0.5, at=seq(0,1, length.out = 32), las=1, cex=1)
    dev.off()
    print('Take a look at the folder to see your roller-coaster...I mean, Captricity picture')
  }
  
  slicing = function(the.picture, cut.direction, cut.index, offset.index, offset=FALSE) {
    if(offset == FALSE) {
      the.slice = ifelse( rep(cut.direction == 'v', dim(the.picture)[1]) ,  the.picture[ , cut.index], the.picture[cut.index, ])
      the.slice = ifelse( (the.slice - 1) < 1, 1, the.slice - 1)
      if(cut.direction == 'v') the.picture[ , cut.index] = the.slice else the.picture[cut.index, ] = the.slice
    }
    else{
      not.1 = ifelse( rep(cut.direction == 'v', dim(the.picture)[1]) ,  the.picture[ , cut.index], pixels[cut.index, ]) != 1
      the.slice = ifelse( rep(cut.direction == 'v', dim(the.picture)[1]), the.picture[ , cut.index + offset.index] + 1 * not.1, the.picture[cut.index + offset.index, ] + 1 * not.1)
      the.slice = ifelse(the.slice > 11, 11, the.slice)
      if(cut.direction == 'v') the.picture[ , cut.index + offset.index] = the.slice else the.picture[cut.index + offset.index, ] = the.slice
    }
    Sys.sleep(.1)
    image(t(apply(the.picture, 2, rev)), col = capcolors, axes=FALSE, main="Let's play a game...")
    mtext(text=1:32, side=1, line=0.5, at=seq(0,1, length.out = 32), las=1, cex=1)
    mtext(text=32:1, side=2, line=0.5, at=seq(0,1, length.out = 32), las=1, cex=1)
    mtext(text=1:32, side=3, line=0.5, at=seq(0,1, length.out = 32), las=1, cex=1)
    mtext(text=32:1, side=4, line=0.5, at=seq(0,1, length.out = 32), las=1, cex=1)
    return(the.picture)
  }
  
  ### play game
  play.game()
  direction = ""
  while(direction != "quit") {
    direction = list(direction.question())
    if (direction == "quit") quitting.time()
    else {
      cut.direction = direction[[1]][[1]]
      cut.index = as.numeric(direction[[1]][[2]])
      pixels = slicing(pixels, cut.direction, cut.index, 0, offset=FALSE)
      pixels = slicing(pixels, cut.direction, cut.index, -1, offset=TRUE)
      pixels = slicing(pixels, cut.direction, cut.index, 1, offset=TRUE)
      pixels = slicing(pixels, cut.direction, cut.index, 0, offset=FALSE)
      pixels = slicing(pixels, cut.direction, cut.index, -1, offset=TRUE)
      pixels = slicing(pixels, cut.direction, cut.index, 1, offset=TRUE)
      pixels = slicing(pixels, cut.direction, cut.index - 1, -1, offset=TRUE)
      pixels = slicing(pixels, cut.direction, cut.index + 1, 1, offset=TRUE)
    }
  }
}





  


  
    
play.capsino = function(){

  countdown.timer = function() {
    count.down3 = matrix(c(
      #1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#6
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#7
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#8
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#9
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#9
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#8
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#7
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#6
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 #1
    ), nrow = 30, byrow = TRUE)
    count.down2 = matrix(c(
      #1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,#6
      1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1,#7
      1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1,#8
      1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1,#9
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1,#0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#9
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#8
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1,#7
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1,#6
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1#1
    ), nrow = 30, byrow = TRUE)
    count.down1 = matrix(c(
      #1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#6
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#7
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#8
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#9
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#9
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#8
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#7
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#6
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1#1
    ), nrow = 30, byrow = TRUE)
    count.down0 = matrix(c(
      #1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#6
      1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#7
      1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,#8
      1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,#9
      1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#0
      1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#1
      1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#0
      1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#9
      1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1,#8
      1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1,#7
      1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,#6
      1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,#5
      1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#4
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#3
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,#2
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1#1
    ), nrow = 30, byrow = TRUE)
    image(t(apply(count.down3, 2, rev)), col = sample(rainbow(10), 2), axes=FALSE, main="Let's play a game...")
    Sys.sleep(1)
    image(t(apply(count.down2, 2, rev)), col = sample(rainbow(10), 2), axes=FALSE, main="Let's play a game...")
    Sys.sleep(1)
    image(t(apply(count.down1, 2, rev)), col = sample(rainbow(10), 2), axes=FALSE, main="Let's play a game...")
    Sys.sleep(1)
    image(t(apply(count.down0, 2, rev)), col = sample(rainbow(10), 2), axes=FALSE, main="Let's play a game...")
    Sys.sleep(1)
  }
  

  monty.hall = function() {
    door.1 = matrix(c(
      #1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 
      1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #1  
      1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #2  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #3  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #4  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #5  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #6  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #7  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #8  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #9  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #0  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #1  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #2  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #3  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #4  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #5  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #6  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #7  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, #8  
      1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, #9  
      1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1  #0  
    ), nrow = 20, byrow = TRUE)
    door.2 = matrix(c(
      #1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 
      1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, #1  
      1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, #2  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, #3  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, #4  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, #5  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, #6  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, #7  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, #8  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, #9  
      1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, #0  
      1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, #1  
      1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, #2  
      1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, #3  
      1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, #4  
      1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, #5  
      1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, #6  
      1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, #7  
      1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, #8  
      1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, #9  
      1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1  #0  
    ), nrow = 20, byrow = TRUE)  
    door.3 = matrix(c(
      #1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 
      1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, #1  
      1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, #2  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #3  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #4  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #5  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #6  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #7  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #8  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #9  
      1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, #0  
      1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, #1  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #2  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #3  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #4  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #5  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #6  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #7  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 1, 1, 1, 1, #8  
      1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, #9  
      1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1  #0  
    ), nrow = 20, byrow = TRUE)
    door.4 = matrix(c(
      #1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 
      1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #1  
      1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #2  
      1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #3  
      1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #4  
      1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #5  
      1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #6  
      1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #7  
      1, 1, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1, 1, #8  
      1, 1, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1, 1, #9  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #0  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #1  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #2  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #3  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #4  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #5  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #6  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #7  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #8  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, #9  
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1  #0  
    ), nrow = 20, byrow = TRUE)
    win.door = matrix(rep(6, 20 * 20), nrow = 20, byrow = TRUE)
    fail.door = matrix(rep(7, 20 * 20), nrow = 20, byrow = TRUE)
    
    make.wall = function(door1, door2, door3, door4) {
      white.separator.1 = matrix(c(
        #1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0, 0, 0, 0, #1  
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #2
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  #3
      ), nrow = 3, byrow = TRUE)
      white.separator.2 = matrix(c(
        6, 2, 6,
        3, 7, 4,
        6, 5, 6
      ), nrow = 3, byrow = TRUE)
      white.separator.3 = cbind(white.separator.1, white.separator.2,
                                white.separator.1)
      black.separator.side = matrix(rep(-1, 43), ncol = 1, byrow = TRUE)
      black.separator.top = matrix(rep(-1, 45), nrow = 1, byrow = TRUE)
      big.wall = cbind(door1, t(white.separator.1), door2)
      big.wall = rbind(big.wall, white.separator.3)
      big.wall = rbind(big.wall, cbind(door3, t(white.separator.1), door4))
      big.wall = cbind(black.separator.side, big.wall, black.separator.side)
      big.wall = rbind(black.separator.top, big.wall, black.separator.top)
      wall.colors = character()
      if(sum(-1 == big.wall) > 0) wall.colors = c(wall.colors, 'black')
      if(sum(0 == big.wall) > 0) wall.colors = c(wall.colors, 'white')
      if(sum(1 == big.wall) > 0) wall.colors = c(wall.colors, 'chocolate')
      if(sum(2 == big.wall) > 0) wall.colors = c(wall.colors, sample(rainbow(10), 1))
      if(sum(3 == big.wall) > 0) wall.colors = c(wall.colors, sample(rainbow(10), 1))
      if(sum(4 == big.wall) > 0) wall.colors = c(wall.colors, sample(rainbow(10), 1))
      if(sum(5 == big.wall) > 0) wall.colors = c(wall.colors, sample(rainbow(10), 1))
      if(sum(6 == big.wall) > 0) wall.colors = c(wall.colors, 'green')
      if(sum(7 == big.wall) > 0) wall.colors = c(wall.colors, 'grey')
      image(t(apply(big.wall, 2, rev)), col = wall.colors, axes=FALSE,
            main="Monty...Magical Doors!")
    }
    
    close.door = function(current.doors, xdoor) {
      current.doors[[xdoor]] = switch(as.numeric(xdoor),
        '1' = fail.door,
        '2' = fail.door,
        '3' = fail.door,
        '4' = fail.door) 
      return(current.doors)
    }
    
    make.wall(door.1, door.2, door.3, door.4)
    Sys.sleep(2)
    print("Riddle me this: Behind this wall are 4 doors")
    Sys.sleep(1)
    print("They can either make you rich or make your poor")
    Sys.sleep(1)
    print("There is but 1 prize")
    Sys.sleep(1)
    print("but here's a surprise")
    Sys.sleep(1)
    print("The winning door pays out its name")
    Sys.sleep(1)
    print("But don't get greedy, that's the game")
    Sys.sleep(1)
    print("After you pick, 2 doors disappear")
    Sys.sleep(1)
    print("For you, it might bring joy or fear")
    Sys.sleep(1)
    print("As nobody knows what's behind the door, not even me!")
    Sys.sleep(1)
    print('So may I ask, what door for thee? 1/2/3/4')
    guess = as.numeric(readline(" "))
    while(guess != 1 & guess != 2 & guess != 3 & guess != 4){
      guess = as.numeric(readline("Try again, 1 thru 4, don't be a boor "))
    }
    
    the.winning.door = sample(1:4, 1)
    eliminate.doors = sample( (1:4)[1:4 != guess & 1:4 != the.winning.door], 2)
    current.doors = list(door.1, door.2, door.3, door.4)
    current.doors = close.door(current.doors, eliminate.doors[1])
    Sys.sleep(2)
    make.wall(current.doors[[1]], current.doors[[2]], current.doors[[3]], current.doors[[4]])
    current.doors = close.door(current.doors, eliminate.doors[2])
    Sys.sleep(2)
    make.wall(current.doors[[1]], current.doors[[2]], current.doors[[3]], current.doors[[4]])
    current.doors[[the.winning.door]] = win.door
    Sys.sleep(1)
    make.wall(current.doors[[1]], current.doors[[2]], current.doors[[3]], current.doors[[4]])
    if(the.winning.door == guess) prize = guess else prize = 0
    if(prize != 0) { print(paste(c("Congrats, you win ", prize,
                                   " dollars! You're rich!"), collapse="")) }
    else { print(paste(c("Sorry, the winning doors was ", the.winning.door, 
                         ". Better luck next time..."), collapse ="")) }
    Sys.sleep(3)
    return(prize)
  }

  play.game = function() {
    play = readline("Are you READY to play the most RADICAL MTurk game in the entire market for MTurk games? y/n ")
    ifelse(play =='y', print("Yup, get ready, it's an endurance run!"), 
           print("Bummer, you've been drafted to play Typing of the Turkers!"))
    Sys.sleep(3)
    print('You are fighting against the clock.')
    Sys.sleep(3)
    print('After the clock starts, Do NOT stop typing until you see your progress report')
    Sys.sleep(3)
    print("There are Gold Standards. The secret is you don't have to get all the words/GS correct")
    Sys.sleep(3)
    print("BUT make sure you have the EXACT number of spaces or else AUTO-FAIL!")
    Sys.sleep(3)
    print("And don't forget, you are being WATCHED...not by a watchman...but by the ever-present CLOCK.")
    Sys.sleep(3)
    print("May the Turker Force be with you!")
    Sys.sleep(6)
    print('')
    print('')
    print('')
  }
  
  generate.check.the.words = function(number.of.words){
    length.of.words = as.integer(runif(number.of.words, 1, 16))
    upper.case = as.integer(runif(sum(length.of.words), 0, 2))
    myletters = letters[sample(1:26, sum(length.of.words), replace = TRUE)]
    myletters = ifelse(upper.case == 0, toupper(myletters), myletters)
    index.of.words = c(1, cumsum(length.of.words)[-(length(length.of.words))] + 1)
    the.words = unlist(lapply(1:length(index.of.words), function(x) paste( myletters[index.of.words[x] : 
      (index.of.words[x] + length.of.words[x] - 1) ], collapse="")))
    number.of.gold.standards = ceiling(number.of.words / 5)
    gold.standard.index = sample(1:number.of.words, number.of.gold.standards)
    gold.standards = the.words[gold.standard.index]
    the.phrase = paste(the.words, collapse=" ")
    countdown.timer()
    print("Please type the following EXACTLY what you see between the quotation marks.")
    print("Special instructions: only LETTERS appear.")
    print(the.phrase)
    the.answer = readline(" ")
    the.answer = unlist(strsplit(the.answer, " "))
    if(length(the.answer) != number.of.words) {
      return (money)
    }
    gs.correct = sum(the.answer[gold.standard.index] == gold.standards)
    gs.incorrect = number.of.gold.standards - gs.correct
    words.correct = sum(the.answer == the.words)
    words.incorrect = sum(the.answer != the.words)
    if(gs.correct < 0.5 * number.of.gold.standards){ 
      passed = 0
      return
    }
    return(c(gs.correct, gs.incorrect, words.correct, words.incorrect))
  }
  
  print.time = function() {
    mycoin = as.character(sample(1:4, 1))
    flipcoin = switch(mycoin,
      '1' = print(paste(c("Live or die. Make your choice...to keep playing. Your time was ", 
        elapsed.time, " seconds."), collapse="")),
      '2' = print(paste(c("Here's some advice--stay alive. Your time was ", 
        elapsed.time, " seconds."), collapse="")),
      '3' = print(paste(c("You think it is over, but the games have just begun. Your time was ", 
        elapsed.time, " seconds."), collapse="")),
      '4' = print(paste(c("Happy MTurker Games--may the odds be ever in your favor. Your time was ", 
        elapsed.time, " seconds."), collapse="")) )
  }
  
  quitting.time = function() {
    the.name = readline("All right! We gotta save YOUR Captricity MTurk score card. What's your name: ")
    the.name = gsub(" " ,"", the.name)
    print(paste("Bobby the Fish sounds cooler. JK. Here you go, ", the.name, "!"))
    barplot(table(c(rep("Words Correct", words.correct), rep("Words Incorrect", 
      words.incorrect), rep("GS Correct", gs.correct), 
      rep('GS Incorrect', gs.incorrect), rep("Round", number.of.words
      - 2), rep("Dollars", money))), main="Congrats! You are a MTurk Survivor!", 
      ylab = "Count", col = sample(rainbow(10), 5))
    jpeg(paste(as.character(the.name), "-loves-MTurk.jpg", sep = ''))
    barplot(table(c(rep("Words Correct", words.correct), rep("Words Incorrect", 
      words.incorrect), rep("GS Correct", gs.correct), 
      rep('GS Incorrect', gs.incorrect), rep("Round", number.of.words
      - 2), rep("Dollars", money))), main="Congrats! You are a MTurk Survivor!", 
      ylab = "Count", col = sample(rainbow(10), 5))
    dev.off()
    print('Take a look at the folder to see your roller-coaster...I mean, MTurk picture')
  }
  
      
  number.of.words = 3
  gs.correct = 0
  gs.incorrect = 0
  words.correct = 0
  words.incorrect = 0
  money = 0
  passed = TRUE

    
  play.game()
  while(passed != 0) {
    elapsed.time = round(system.time(( results = list(
      generate.check.the.words(number.of.words)) ))[3], 2)
    Sys.sleep(1)
    print.time()
    if(elapsed.time > 60 | results[[1]][1] == 0) {
      if(elapsed.time > 60) print('Sorry, you were a bit slow--ran out of time')
      if(results[[1]][1] == 0) {
        print('Darn, you either did not have the EXACT number of spaces, ')
        print('or you got too many Gold Standards wrong for this round')
        print('But hey! Thanks for playing.')
      }
      quitting.time()
      return (0)
    }
    gs.correct = gs.correct + results[[1]][1]
    gs.incorrect = gs.incorrect + results[[1]][2]
    words.correct = words.correct + results[[1]][3]
    words.incorrect = words.incorrect + results[[1]][4]
    money = money + monty.hall()
    barplot(table(c(rep("Words Correct", words.correct), rep("Words Incorrect", 
      words.incorrect), rep("GS Correct", gs.correct), 
      rep('GS Incorrect', gs.incorrect), rep("Round", number.of.words
      - 2), rep("Dollars", money))), main="Congrats... You are doing well!", 
      ylab = "Count", col = sample(rainbow(10), 5))
    Sys.sleep(5)
    number.of.words = number.of.words + 1
  }
}


