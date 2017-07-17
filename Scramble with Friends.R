direction=function(word, used, position, myLetters) {
  one=c(2,5,6)
  two=c(1,3,5,6,7)
  three=c(2,4,6,7,8)
  four=c(3,7,8)
  five=c(1,2,6,9,10)
  six=c(1,2,3,5,7,9,10,11)
  seven=c(2,3,4,6,8,10,11,12)
  eight=c(3,4,7,11,12)
  nine=c(5,6,10,13,14)
  ten=c(5,6,7,9,11,13,14,15)
  eleven=c(6,7,8,10,12,14,15,16)
  twelve=c(7,8,11,15,16)
  thirteen=c(9,10,14)
  fourteen=c(9,10,11,13,15)
  fifteen=c(10,11,12,14,16)
  sixteen=c(11,12,15)
  
  if(position==1) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(one) != sum(one %in% used) ) {
      position=sample( rep( one[ !(one %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==2) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(two) != sum(two %in% used) ) {
      position=sample( rep( two[ !(two %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==3) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(three) != sum(three %in% used) ) {
      position=sample( rep( three[ !(three %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==4) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(four) != sum(four %in% used) ) {
      position=sample( rep( four[ !(four %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==5) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(five) != sum(five %in% used) ) {
      position=sample( rep( five[ !(five %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==6) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(six) != sum(six %in% used) ) {
      position=sample( rep( six[ !(six %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==7) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(seven) != sum(seven %in% used) ) {
      position=sample( rep( seven[ !(seven %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==8) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(eight) != sum(eight %in% used) ) {
      position=sample( rep( eight[ !(eight %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==9) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(nine) != sum(nine %in% used) ) {
      position=sample( rep( nine[ !(nine %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==10) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(ten) != sum(ten %in% used) ) {
      position=sample( rep( ten[ !(ten %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==11) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(eleven) != sum(eleven %in% used) ) {
      position=sample( rep( eleven[ !(eleven %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }    
  
  else if(position==12) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(twelve) != sum(twelve %in% used) ) {
      position=sample( rep( twelve[ !(twelve %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==13) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(thirteen) != sum(thirteen %in% used) ) {
      position=sample( rep( thirteen[ !(thirteen %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }  
  
  else if(position==14) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(fourteen) != sum(fourteen %in% used) ) {
      position=sample( rep( fourteen[ !(fourteen %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }  
  
  else if(position==15) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(fifteen) != sum(fifteen %in% used) ) {
      position=sample( rep( fifteen[ !(fifteen %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
  else if(position==16) {
    word=paste(word, myLetters[position], sep="")
    used=c(used,position)
    if(length(sixteen) != sum(sixteen %in% used) ) {
      position=sample( rep( sixteen[ !(sixteen %in% used) ] ,2) ,1)
      return( c(word,used,position) )
    }
    else return ( c(word,used,17) )
  }
  
}

oneWord=function( inputLetters,paramOfWord=parameterOfWord )  {
  myLetters=unlist(strsplit(inputLetters,"") )
  word=character()
  wordLength=sample(paramOfWord[1]:paramOfWord[2],1)
  used=numeric()
  position=sample(1:16,1)
  for(i in 1:wordLength) {
    results=direction(word=word, used=used, position=position, myLetters=myLetters)
    word=results[1]
    used=results[2:(i+1)]
    position=as.numeric(results[i+2])
    if(position==17) return (results[1:(i+1)])
  }
  return(results[1:(i+1)])
}


dictionary = function(paramOfWord, listOfWords,numbers) {
  smallDictionary = readLines( "small_dictionary.txt" )
  bigDictionary = readLines("big_dictionary.txt")  
  
  cutter=function(x) {
    if( nchar(x)>=paramOfWord[1] & nchar(x)<=paramOfWord[2] ) return(x)
  }
  
  myDictionary=unlist( lapply(bigDictionary, cutter) )
  
  checker = function(x, dic=myDictionary) {
    if(x %in% dic) return (x)
  }
  
  realWords=unlist(lapply(listOfWords, checker))
  numbers=numbers[which(listOfWords %in% realWords),]
  numbers=numbers[order(nchar(realWords), decreasing=T),]
  realWords=realWords[order(nchar(realWords) , decreasing=T)]
  for(i in 1:length(realWords)) {
    print( c(realWords[i],na.omit(numbers[i,]) ) )
  }
}

scrambleWithFriends = function(inputLetters=letters[1:16],
                               parameterOfWord=c(4,7), cycles=1000) {
  
  numbers = matrix(nrow=cycles,ncol=parameterOfWord[2])
  wordList = character()
  for (i in 1:cycles) {
    result = oneWord(inputLetters, parameterOfWord)
    wordList[i] = result[1]
    numbers[i,1:length(result)-1] = result[2:length(result)]
  }
  
  dictionary(paramOfWord=parameterOfWord, listOfWords=wordList, numbers)
  
}

scrambleWithFriends("clbruinesremteac", c(3,7), cycles=1000)