## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#this is the makeCache function it is like an empty box to be filled with the matrix inverse at any moment
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  #if m is not null then the inverse matrix has been already calculated we can return the cached data
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #otherwise let's solve the inverse directly here and set it in order to store the result
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

