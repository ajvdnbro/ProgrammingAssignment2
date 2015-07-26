## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.


## create a function called ‘makeVector’ with an empty numeric vector as default value
## set 'solv' as an empty vector
## create a function called ‘set’ 
## The special assignment operator <<- is for making an assignment within a function (you can also use assign()). 
#It is used to change the value associated with f.ex. x. This operator looks back in enclosing environments for an environment 
#that contains the symbol x and when it finds such an environment it replaces the value, in that environment, with the value of 
#right hand side. If the global or top-level environment is reached without finding the symbol x then that variable is created 
#and assigned to there. 
## create a function called ‘get’, that has no arguments and reproduces ‘x’
## create a function ‘setmean’ to calculate the inverse of the matrix (solve); solve is assigned
## create a function ‘getmean’, that has no arguments and reproduces ‘solv’
## create a list: set – get – setmean – getmean

makeCacheMatrix <- function(x = matrix()) {
  solv <- NULL
  
  set <- function(y) {
    x <<- y					
    solv <<- NULL
  }
  get <- function() {x}

  setinverse <- function(solve) {solv <<- solve}
  getinverse <- function() {solv}
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## create a function ‘cachemean’ with argument x and … for possible other arguments
## set 'solv' as the list where data of ‘getinverse’ stands
## if 'solv' is not(!) an empty vector – create the message : “getting cached data”, and return the value of 'solv'
## select 'get' of the list and put it in 'data'
## inverse the data
## print vector 'solv'



cacheSolve <- function(x, ...) {
  solv <- x$getinverse()

  if(!is.null(solv)) {
    message("getting cached data")
    return(solv)
  }
  
  data <- x$get()					
  solv <- solve(data, ...)
  x$setinverse(solv)				
  solv
}
