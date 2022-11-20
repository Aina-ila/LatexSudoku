
# Step 1: Define the board -----------------------------------------------------

puzzle <- c(5,3,0, 0,7,0, 0,0,0,
            6,0,0, 1,9,5, 0,0,0,
            0,9,8, 0,0,0, 0,6,0,
            
            8,0,0, 0,6,0, 0,0,3,
            4,0,0, 8,0,3, 0,0,1,
            7,0,0, 0,2,0, 0,0,6,
            
            0,6,0, 0,0,0, 2,8,0,
            0,0,0, 4,1,9, 0,0,5,
            0,0,0, 0,8,0, 0,7,9)


# Step 2: Convert puzzle into matrix --------------------------------------------

puzzle <- matrix(puzzle, nrow = 9, ncol = 9, byrow = TRUE) 


# Step 3: Finding the possible answers for each boxes---------------------------

# Possible answer denoted by "Q".

Q <- function(puzzle, m, n){
  Q <- rep(TRUE,9)
  selected_num <- unique(c(puzzle[m,], 
                           puzzle[,n], 
                           puzzle[3*((m-1) %/% 3) + 1:3, 
                                  3*((n-1) %/% 3) + 1:3]))
  
  selected_num <- na.omit(selected_num)
  Q[selected_num] <- FALSE
  
  return(Q)
}


# Step 4: Determine solution through iteration ---------------------------------

# 'puzzle' argument provides the matrix, length 81 (9x9), to iterate through. 
# 'progress' argument provides a starting value to recursively iterate through.

solve <- function(puzzle, progress = 81) {
  if  (0 %in% puzzle) {
    puzzle[puzzle == 0] <- NA
  } else puzzle
  
  if (progress == 0) {
    return(puzzle)
  }
  
  # Get the m,n coordinates
  
  m <- ((progress - 1) %% 9) + 1 
  n <- ((progress - 1) %/% 9) + 1 
  
  if (is.na(puzzle[m, n])) {
    choices <- which(Q(puzzle, m, n))
  } else{
    choices <- c(puzzle[m, n])
  }
  
  for (k in choices) {
    puzzle[m, n] <- k
    
    answer <- solve(puzzle, progress - 1)
    
    if (!is.null(answer)) {
      return(answer)
    }
  }
  return(NULL)
}


# Step 5: ----------------------------------------------------------------------

solve(puzzle)
