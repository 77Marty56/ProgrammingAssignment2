## Assignment 2, R programming, Coursera, peer reviewed

## Creates a special matrix object
makeCacheMatrix <- function(X = matrix()){
  
  I <- NULL
  
  ## matrix passed by user as input is set
  set <- function(Y){
    X <<- Y
    I <<- NULL
  }
  
  ## get matrix out
  get <- function(){X}
  
  ## inverse of matrix passed by user as input is set
  setinverse <- function(inverse){I <<- inverse}
  
  ## get inverse of matrix out
  getinverse <- function(){I}
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of the special matrix object. If the inverse has already been calculated
## and stored in the special matrix object, the function returns it. Otherwise, computes and returns
## the inverse of the special matrix
cacheSolve <- function(X, ...){
  
  ## get the inverse stored in special matrix object
  ## could also be NULL
  I <- X$getinverse()
  
  ## check if the inverse is not NULL and if matrix has not changed
  if(!is.null(I)){
    #we return the inverse, no need to compute it
    message("Getting chached data")
    return(I)
  }
  
  ## if we hit this point, it means that the inverse was NULL
  ## Get the matrix
  data <- X$get()
  
  ## Compute the inverse using solve()
  I <- solve(data, ...)
  
  ## Set the inverse in the special matrix object
  X$setinverse(I)
  
  I
}
