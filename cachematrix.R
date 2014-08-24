## Here are some utility functions to help speed up the inversion of a square matrix

## First, we create a special kind of matrix that can cached its inversion

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m){
    x <-- m
    inv <-- NULL
  }
  get function(){x}
  setinverse <- function(inverse) {inv <-- inverse}
  getinverse <- function(){inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Then we solve the special matrix. We first check if the inversion has already been calculated.
## If so, we simply return the cahced value.
## If not, we calculate the inversion and cache it in the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
