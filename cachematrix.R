## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value of the matrix and clear old values
  set <- function(y = matrix()) {
    x <<- y #set the value
    m <<- NULL #clear old values
  }
  get <- function() x
  #set inverse
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  #return list with the functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## A function to return the inverse matrix for matrix x

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() # gets the saved/cached value for x
  if(!is.null(m)) { # if the cache is not NULL eg. empty, the cached value is returned
    message("getting cached data")
    return(m)
  }
  #If the cache was empty, we need to calculate the inverse value
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
 
