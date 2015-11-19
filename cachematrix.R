## These two functions turn an input matrix into a
## "cache matrix" that can hold its inverse in cache
## for easy retrieval once it's been calculated.

## makeCacheMatrix takes an input matrix and creates a vector object
## that is functionally the same input matrix, plus the ability to
## store the matrix's inverse in a cached value.

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve first checks to see if there is already a cached
## inverse for "cache matrices" from the above function, and
## returns that result. Otherwise, it calculates the inverse
## and caches it for later retrieval.

cacheSolve <- function(x, ...)
{
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m  
    ## Return a matrix that is the inverse of 'x'
}
