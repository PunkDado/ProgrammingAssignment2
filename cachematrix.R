## These are a pair of functions that take a matrix and invert it
## In the first time a matrix is inverted, the result gets stored
## in the cache
## If the matrix has been already inverted once, the result is gotten
## out of the cache

## makeCacheMatrix() makes a list of functions that verifies if the 
## matrix has been previously inverted

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                       
        ## assign NULL value to m; m is the variable that will be used by cacheSolve()
        ## to evaluate the matrix "x" has been already inverted
        
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


## cashSolve() uses makeCacheMatrix to check if a matrix was
## used previously. If it wasn't, then the funcion inverts the matrix.
## Otherwise it displays the message "getting cached data" and the result (that was
## previously calculated).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()       ## Assigns to m in the function's envenronment the value of m
                                ## stored in the global environment
        
        if(!is.null(m)) {       ## if the value assigned to m by x$getsolve() is NOT NULL,
                                ## then the inversion was previously done, and then cacheSolve()
                                ## returns the value cached in m
                
                message("getting cached data")
                return(m)
        }
        data <- x$get()         ## Assigns the value of the matrix to data
        
        m <- solve(data, ...)   ## Inverts the matrix x and assigns the result do m in the 
                                ## environment of cacheSolve()
        
        x$setsolve(m)           ## Assigns m in the environment of makeCashMatrix() 
                                ## with the result of the inversion
        
        m                       ## Prints the result
}