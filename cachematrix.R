## Below functions will setup a list of functions for a given
## matrix and will calculate / read(from cache) the inverse 
##of the matrix

## This function will setup functions, for the passed in matrix
## which would help during the matrix inverse calculateion/
## storage in cache

makeCacheMatrix <- function(x = matrix()) {
  inverseValue <- NULL
  
  set <- function(y){
    x <<- y
    inverseval <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inverseValue <<- inverse
  getinverse <- function() inverseValue
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will calculate the inverse of a matrix.
## Before calculating the inverse, it checks if the
## inverse of the matrix was calculated previously,
## if so then it uses that and return, otherwise it
## will calculate the inverse, sets it in the cache
## and returns the value

cacheSolve <- function(x, ...) {
  cachedInverse <- x$getinverse()
  
  if(!is.null(cachedInverse)){
    message("found inverse value in cache")
    return(cachedInverse)
  }
  
  data <- x$get()
  
  inverseVal <- solve(data,...)
  
  x$setinverse(inverseVal)
  
  inverseVal        
}
