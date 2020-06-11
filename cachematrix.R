## makeCacheMatrix and cacheSolve functions create a matrix and return the inverse of the matric and cache it
## for future reference. This helps save computation time as inverse operation is compute intensive on
##large matrices. The cacheSolve function computes inverse only when the iverse is not calculated else returns
##the cached value of the inverse matrix


## Function creating an input matrix and creating functions to cache the matrix and its inverse in a list

makeCacheMatrix <- function(y = matrix(), inv_mtx=matrix()) {
  #m <- NULL
  setmatrix <- function() {
    
    if(all(is.na(y))){
      print("You passed a Null matrix")
    }
    else if(any(y!=x))
    {
      x<<- y
      m <<- NULL
      print("Matrix Created")
    }
    
  } 
  
  getmatrix <- function() x
  setinverse <- function() m <<- inv_mtx
  getinverse <- function() m
  lst <<- list(set = setmatrix(), get = getmatrix(),
               setinverse = setinverse(),
               getinverse = getinverse())
  print("list updated and inverse matrix returned")
  rm(inv_mtx)
  return(lst[["getinverse"]])
  
}


## Function checks if the inverse exists and returns the cached inverse else computes inverse of the
##matrix

cacheSolve <- function(mtx=x, lt=list()) {
  ## Return a matrix that is the inverse of 'x'
  
  #######Checks 2 conditions
  ### 1: if the input matrix is the same as the previous matrix
  ### 2: If input matrix and and its inverse have been cached in a list
  m <-NULL
  if (exists("lst") && all(mtx==lst$get)) {
    m <- lst$getinverse
    print('No Change in input matrix')
  }
  if(!is.null(m)) {
    message("getting cached value of existing inverse matrix")
    print(m)
    return(m)
  }
  ######Else Create the inverse of the matrix and cache the new matrix and its inverse in a list and returns
  #### the inverse matrix
  else {
    x <- as.matrix(mtx)
    m <- solve(x)
    inv_mtx <- makeCacheMatrix(x, m)
  }
  rm(list=c('mtx', 'm'))
  return(inv_mtx)
  
}

#####Calling the functions

inv_matrix <- cacheSolve(x)

#######Removing redundant objects
rm(m)
