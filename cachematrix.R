## The following pair of functions help create, calculate and 
## retrive a matrix and its inverse stored in cache

##____________________________________________________________________________________

## The following functions makes the special matrix that stores the inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

##_________________________________________________________________________________

## The following function checks if the inverse is stored in the cache, if yes, it 
##returns it else it calculates it, stores it in cache and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
