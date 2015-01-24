## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function stores a matrix object and a single
## cached value.
makeCacheMatrix <- function(x = matrix()) {
  ## Generic value that you want to cache
  cache <- NULL
  ## Setter. Sets argument to 'x'. Clear cache
  set <- function(y) {
          x <<- y
          ## Clear cache
          cache <<- NULL
  }
  ## Getter
  get <- function() x
  setcache <- function(val) cache <<- val
  getcache <- function() cache
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## This function queries the object associated with
## a matrix for a valid cache value. If present, it
## returns it, else it calculates the inverse of
## the objects data (i.e. the matrix), stores it in
## the cache and returns the calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getcache()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setcache(inv)
        inv
}

