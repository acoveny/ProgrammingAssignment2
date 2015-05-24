## A pair of functions which cache the inverse of a matrix.
##
## Usage :
## > mym <- matrix( 1:4, nrow = 2, ncol = 2)
## > mc <- makeCacheMatrix(mym)
## > mi <- cacheSolve( mc )
##
## mi is the inverse of supplied matrix mym
## mc is the matrix cache

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	    m <- NULL

	    set <- function(y) {
		      x <<- y
		      m <<- NULL
	    } 

	    get <- function() x
	    setinv <- function(solve) m <<- solve
	    getinv <- function() m

  	  list ( set = set, get = get, setinv = setinv, getinv = getinv  )
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()

        if (!is.null(m)) {
        	## message("getting cached data")
        	return(m)
        }
        
        data <- x$get()

        m <- solve( data, ... )

        x$setinv(m)
        
        m
}
