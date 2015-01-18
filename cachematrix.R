## The functions below create a special cache matrix then calculates the inverse of a 
## matrix and stores it in the cache

## makeCacheMatrix creates a special cache matrix where the matrix can be stored

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(x){
          x <<- y
          m <<- NULL
  }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


## cacheSolve calculates the inverse of the matrix from the above function.  
## if the inverse has already been calculated then the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)){
          message("getting cached data")
          return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
      m
}
