conductor = function(size = 40, foods = 13, energy = FALSE, erase.tail=TRUE, 
  move.diagonal = FALSE, grow = TRUE, wait = TRUE) {

  # size: n x n sized grid as the stage
  # foods: number of food tiles
  # energy: number of default steps for snakes before they cannot move;
  #   after reaching a food tile, that snake will increase its energy
  # erase.tail: whether or not to erase the tail after it has moved beyond a tile
  # move.diagonal: ability for the snakes to move diagonally
  # grow: whether or not a snake's body will increase in length after eating a food
  # wait: whether you want to watch the whole game play out or only the conclusion
  
  set.up.the.stage = function(size. = size, foods. = foods){
    coordinates = sample(seq(size.), foods. * 2, replace=T)
    the.grid = matrix(rep(0, size. * size.), nrow=size.)
    gem.coord.mat = matrix(coordinates, ncol=2, byrow=T)
    gem.coord.mat = cbind(gem.coord.mat, rep(0, foods.))
    
    for(i in  seq(length(coordinates) / 2)) {
      if (i == ceiling(foods. / 2)) {
        the.grid[coordinates[(2 * i) - 1], coordinates[2 * i]] = 1 #midpoint is the same color as starting
        gem.coord.mat[i, 3] = 1
      }
      else if ((i - 1) %% 4 == 0) {
        the.grid[coordinates[(2 * i) - 1], coordinates[2 * i]] = 1
        gem.coord.mat[i, 3] = 1
      }
      else if ((i - 1) %% 4 == 1) {
        the.grid[coordinates[(2 * i) - 1], coordinates[2 * i]] = 2
        gem.coord.mat[i, 3] = 2
      }
      else if ((i - 1) %% 4 == 2) {
        the.grid[coordinates[(2 * i) - 1], coordinates[2 * i]] = 3
        gem.coord.mat[i, 3] = 3
      }
      else if ((i - 1) %% 4 == 3) {
        the.grid[coordinates[(2 * i) - 1], coordinates[2 * i]] = 2
        gem.coord.mat[i, 3] = 2
      }
    }
    return(list(the.grid, gem.coord.mat))
    #E S C S   E   S C S E
    #1 2 3 4   5   6 7 8 9 Multiple of 4 + 1
  }
  
  point.of.snake.head = function(size. = size) {
    x.coord = 0
    y.coord = 0
    statement = paste("A number between 2 and", size. - 1, "inclusive.")
    print("Where do you want to put the head of your train?")
    x.coord = as.integer(readline(
      paste("Conductor, the x-coordinate, please.", statement, "> ")))
    while(x.coord < 2 | (size. - 1) < x.coord) x.coord = 
      as.integer(readline(paste("MrDrProfessor Engineer:", statement, "> ")))
    y.coord = as.integer(readline(
      paste("Conductor, the y-coordinate, please.", statement, "> ")))
    while(y.coord < 2 | (size. - 1) < y.coord) y.coord = 
      as.integer(readline(paste("MrDrProfessor Engineer:", statement, "> ")))
    return(c(x.coord, y.coord))
  }
  
  put.snake.body = function(snake.head, which_snake) {
    for(i in -1:1) {
      for(j in -1:1) {
        the.grid[snake.head[1] + i, snake.head[2] + j] = which_snake + 3
      }
    }
    return(the.grid)
  }
  
  make.snake = function(snake.head) {
    snake.body = c(snake.head[1], snake.head[2], #5
                   snake.head[1], snake.head[2] + 1, #2
                   snake.head[1] - 1, snake.head[2] + 1, #1
                   snake.head[1] - 1, snake.head[2], #4
                   snake.head[1] - 1, snake.head[2] - 1, #7
                   snake.head[1], snake.head[2] - 1, #8
                   snake.head[1] + 1, snake.head[2] - 1, #9
                   snake.head[1] + 1, snake.head[2], #6
                   snake.head[1] + 1, snake.head[2] + 1) #3
  }

  find.gem = function(snake.head, gem.coordinates, size. = size, diag = move.diagonal) {
    
    add.direc = function(vec, direction) {
      vec[1] = vec[1] + direction[1]
      vec[2] = vec[2] + direction[2]
      return(vec)
    }
    
    bump = function(snake.head, direction) {
      problem = c(0, 0)
      for (i in seq(length(snake.head))) {
        if (snake.head[i] == 1) problem[i] = 1
        else if (snake.head[i] == size.) problem[i] = 2
      }
      
      invalid.directions = numeric()
      invalid.directions = switch(as.character(problem[1]),
                                  '0' = invalid.directions,
                                  '1' = c(invalid.directions, 1, 4, 7),
                                  '2' = c(invalid.directions, 3, 6, 9)) 
      invalid.directions = switch(as.character(problem[2]),
                                  '0' = invalid.directions,
                                  '1' = c(invalid.directions, 7, 8, 9),
                                  '2' = c(invalid.directions, 1, 2, 3)) 
      
      if(direction %in% invalid.directions) {
        direction = find.gem(snake.head, gem.coordinates)
        direction = bump(snake.head, direction)
      }
      return(direction)
    }
    
    distance = numeric(9) + 2 * size.
    distance[2] = dist(rbind(add.direc(snake.head, c(0, 1)), gem.coordinates)) #2
    distance[4] = dist(rbind(add.direc(snake.head, c(-1, 0)), gem.coordinates)) #4
    distance[5] = dist(rbind(add.direc(snake.head, c(0, 0)), gem.coordinates)) #5 weird case
    distance[6] = dist(rbind(add.direc(snake.head, c(1, 0)), gem.coordinates)) #6
    distance[8] = dist(rbind(add.direc(snake.head, c(0, -1)), gem.coordinates)) #8
    direction = sample(rep(order(distance), c(85, 10, 5, 0, 0, 0, 0, 0, 0)), 1)
    # if food is in ordinal direction, then 10% chance of doing nothing
    # if food is in non-ordinal direction, then 5% of doing nothing
    
    if(diag) {
      distance[1] = dist(rbind(add.direc(snake.head, c(-1, 1)), gem.coordinates)) #1
      distance[3] = dist(rbind(add.direc(snake.head, c(1, 1)), gem.coordinates)) #3
      distance[7] = dist(rbind(add.direc(snake.head, c(-1, -1)), gem.coordinates)) #7
      distance[9] = dist(rbind(add.direc(snake.head, c(1, -1)), gem.coordinates)) #9
      direction = sample(rep(order(distance), c(70, 10, 10, 5, 5, 0, 0, 0, 0)), 1)
      # almost always 5% chance of doing nothing if able to move diagonally
    }
    
    
    direction = bump(snake.head, direction)
    #recursive; continues until finds valid move, cannot crash into wall
    return(direction)
  }
  
  move.head = function(snake.head, direction) {
    new.coordinates = switch(as.character(direction),
      '1' = c(snake.head[1] - 1, snake.head[2] + 1),
      '2' = c(snake.head[1], snake.head[2] + 1),
      '3' = c(snake.head[1] + 1, snake.head[2] + 1),
      '4' = c(snake.head[1] - 1, snake.head[2]),
      '5' = c(snake.head[1], snake.head[2]), #strange case
      '6' = c(snake.head[1] + 1, snake.head[2]),
      '7' = c(snake.head[1] - 1, snake.head[2] - 1),
      '8' = c(snake.head[1], snake.head[2] - 1),
      '9' = c(snake.head[1] + 1, snake.head[2] - 1) ) 
    return(new.coordinates)
  }
  
  move.snake = function(snake.body, which_snake, erasetail = erase.tail) {
    if(erasetail) the.grid[snake.body[length(snake.body) - 1],
      snake.body[length(snake.body)]] = 0
    for(index in seq(1, 18, 2)) {
      the.grid[snake.body[index], snake.body[index + 1]] = which_snake + 3
    }
    return(the.grid)
  }
  
  snake.match = function(snake.body, specific.gem.coordinates) {
    if (is.na(specific.gem.coordinates[1])) return(0)
    else if(snake.body[1] == specific.gem.coordinates[1] & 
            snake.body[2] == specific.gem.coordinates[2]) return(1)
    else return(0)
  }
  
  updated.matrix = function(mat, keep) {
    if(keep == "head") return(matrix(mat[-nrow(mat), ], ncol=3))
    if(keep == "tail") return(matrix(mat[-1, ], ncol=3))
  }
  # to update matrix, if matrix is subseted to be 1 row, it becomes
  # a vector automatically, but we still need the properties to be matrix
  # This is primarily to deal with redrawing foods if traveled on but not eaten.
  
  redraw.grid = function(thegrid, gem.mat) {
    if(nrow(gem.mat) == 0) return(thegrid)
    
    for(i in seq(nrow(gem.mat))) {
      condition = thegrid[gem.mat[i, 1], gem.mat[i, 2]] %in% c(4, 5)
      if(!condition) thegrid[gem.mat[i, 1], gem.mat[i, 2]] = gem.mat[i, 3]
    }
    return(thegrid)
  }
  
  size = size # size of the grid/stage
  foods = foods # number of food tiles
  energy = energy # initialize number of steps snake can move to get to 1st food
  erase.tail = erase.tail # whether or not snake's tail is erased or not
  move.diagonal = move.diagonal # ability for snake to move in all directions
  
  the.color = c('lightgrey', 'red', 'green', 'magenta', 'blue', 'purple')
  results = set.up.the.stage()
  grid.stepped.on = matrix(rep(0, size * size), nrow=size)
  the.grid = results[[1]]
  gem.coordinates.matrix = results[[2]]
  player.1 = player.2 = 0
  if (energy) energy.1 = energy.2 = energy
  else energy.1 = energy.2 = size #energy is an implicit argument
  # idea is to have it dynamically adjusted
  image(the.grid, col = the.color, axes=FALSE, main=".")
  Sys.sleep(.5)
  
  snake.head.1 = point.of.snake.head(size)
  snake.head.2 = point.of.snake.head(size)
  snake.body.1 = make.snake(snake.head.1)
  snake.body.2 = make.snake(snake.head.2)
  the.grid = put.snake.body(snake.body.1, 1)
  the.grid = put.snake.body(snake.body.2, 2)

  image(the.grid, col = the.color, axes=FALSE, main=".")
  Sys.sleep(4)
  
  while(nrow(gem.coordinates.matrix) & (energy.1 | energy.2)) {
    if (energy.1 > 0){
      direction.1 = find.gem(snake.head.1, gem.coordinates.matrix[1, 1:2])
      snake.head.1 = move.head(snake.head.1, direction.1)
      grid.stepped.on[snake.head.1[1], snake.head.1[2]] = grid.stepped.on[
        snake.head.1[1], snake.head.1[2]] + 1
      snake.body.1 = c(snake.head.1, snake.body.1)
      the.grid = move.snake(snake.body.1, 1)
      if(!snake.match(snake.head.1, gem.coordinates.matrix[1, 1:2]) | 
         !grow) snake.body.1 = head(snake.body.1, length(snake.body.1) - 2)
      energy.1 = energy.1 - 1
      #print(energy.1)
    }
    
    if (energy.2 > 0){
      direction.2 = find.gem(snake.head.2, gem.coordinates.matrix[nrow(
        gem.coordinates.matrix), 1:2])
      snake.head.2 = move.head(snake.head.2, direction.2)
      grid.stepped.on[snake.head.2[1], snake.head.2[2]] = grid.stepped.on[
        snake.head.2[1], snake.head.2[2]] + 1
      snake.body.2 = c(snake.head.2, snake.body.2)
      the.grid = move.snake(snake.body.2, 2)
      if(!snake.match(snake.head.2, gem.coordinates.matrix[nrow(
        gem.coordinates.matrix), 1:2]) | !grow) snake.body.2 = head(
          snake.body.2, length(snake.body.2) - 2)
      energy.2 = energy.2 - 1
      #print(energy.2)
    }

    the.grid = redraw.grid(the.grid, gem.coordinates.matrix) # gems will reappear
    if(wait) {
      image(the.grid, col = the.color, axes=FALSE, main=".")
      Sys.sleep(.25)
    }
    
    if(snake.match(snake.head.1, gem.coordinates.matrix[1, 1:2])) {
      gem.coordinates.matrix = updated.matrix(gem.coordinates.matrix, "tail")
      player.1 = player.1 + 1
      energy.1 = energy.1 + as.integer(size * .75)
    }
    
    if(snake.match(snake.head.2, gem.coordinates.matrix[nrow(
      gem.coordinates.matrix), 1:2])) {
      gem.coordinates.matrix = updated.matrix(gem.coordinates.matrix, "head")
      player.2 = player.2 + 1
      energy.2 = energy.2 + as.integer(size * .75)
    }

  }
    
  scores = paste("P1 Score:", player.1, "; P2 Score:", player.2)
  steps = paste("Steps left: P1:", energy.1, "; P2:", energy.2)
  if(player.1 > player.2) {
    image(the.grid, col = the.color, axes=FALSE, main=paste("Player 1 Wins!", 
      scores), xlab=steps)
  } else if(player.1 < player.2) {
    image(the.grid, col = the.color, axes=FALSE, main=paste("Player 2 Wins!", 
      scores), xlab=steps)
  } else {
    image(the.grid, col = the.color, axes=FALSE, main=paste("Tie!", scores),
      xlab=steps)
  }
  
  Sys.sleep(3)
  return(grid.stepped.on)
}

# hint: look at all the possible arguments for conductor function
# hint: notice what is usually the color of the first food tile
# hint: notice what is usually the color of the midpoint food tile
grid.stepped.on = conductor(size=21, foods = 30, energy = 150, erase.tail = T, 
    grow=F, wait = T) #ideally foods is (multiple of 4) + 1
#image(grid.stepped.on)
#contour(seq(ncol(grid.stepped.on)), seq(nrow(grid.stepped.on)), grid.stepped.on)
#persp(seq(ncol(grid.stepped.on)), seq(nrow(grid.stepped.on)), grid.stepped.on, 
#      theta =30, phi =40)

