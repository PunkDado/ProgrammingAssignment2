## These are a pair of functions that take a matrix and invert it
## In the first time a matrix is inverted, the result gets stored
## in the cache
## If the matrix has been already inverted once, the result is gotten
## out of the cache

## makeCacheMatrix() makes a list of functions that verifies if the 
## matrix has been used previously

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
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


## cashSolve() uses MakeCacheMatrix to check if a matrix was
## used previously. If it wasn't, then the funcion inverts the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
