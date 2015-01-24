## This function stores a matrix object and a single
## cached value. It contains functions to store and
## query the matrix, and the cache value. The cache
## can be used to store any single computed result
## from the matrix.

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

  ## Setter and Getter of the cache values
  setcache <- function(val) cache <<- val
  getcache <- function() cache

  ## Return the object
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

