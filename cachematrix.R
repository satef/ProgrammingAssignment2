## we want to create a global variable and cache in it the value of inverse of a matrix 
## this matrix will be first check if it is cached before or not, then will invert it and cache it
## functions do

## this function responsible about caching the inverted matrix
##and provide getter and setter functions for accessing it

makeCachedMatrix <- function(x = matrix()) {
  cachedMatrix <- NULL
  set <- function(y) {
    x <<- y
    cachedMatrix <<- NULL
  }
  get <- function() x
  setCache <- function(inverse) cachedMatrix <<- inverse
  getCache <- function() cachedMatrix
  list(set = set,
       get = get,
       setCache = setCache,
       getCache = getCache)
}


## this function is responsible about returnning a matrix that is the inversion of x
## check first if it is cached, if not cache it and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		mm <- x$getCache()
		if (!is.null(mm)) {
			message("loading cached matrix...")
			return(mm)
		}
		else {
    matrix <- x$get()
    mm <- solve(matrix, ...)
    x$setCache(mm)
    return(mm)
  }
}
