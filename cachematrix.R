## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inv <<- inv
  getInv <- function() m
  list(set = set,
    get = get,
    setInv = setInv,
    getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'       
        inv <- x$getInv()
        # If inverse of matrix has already been calculated, return it.
        if (!is.null(inv)) {
                return(inv)
        }
        # Otherwise, get the matrix, solve it for it's inverse and set it in
        #   the matrix object.
        given_matrix <- x$get()
        inv <- solve(given_matrix, ...)
        x$setInv(inv)
        ## Return the matrix object with the inverse of 'x'
        inv
}
