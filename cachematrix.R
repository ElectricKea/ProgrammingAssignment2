## These 2 functions will calculate the inverse of a matrix.
## The inverse will then be stored in cache so that, if the matrix inverse
## is needed again, it will not have to recalculate it.

## The makeCacheMatrix function calcuates the inverse of a matrix.
## The cache will be cleared and a new answer to the matrix inverse will be stored.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_matrix <- function(solve) m <<- solve
  get_matrix <- function() m
  list(set = set, get = get,
       set_matrix = set_matrix,
       get_matrix = get_matrix)

}


## The cacheSolve function calculates the inverse of the matrix by using the information
## stored in cache by the makeCacheMatrix function. If the inverse of the matrix has already
## be calcuated it will return the value stored in the cache. If not, the matrix inverse will be
## calcuated.

cacheSolve <- function(x, ...) {
  m <- x$get_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_matrix(m)
  m
}
