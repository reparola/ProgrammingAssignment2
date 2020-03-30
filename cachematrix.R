## This pair of functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #clears the m variable which will store the inverse matrix
        set <- function(y) { #defines the function set which will update the 
                        #matrix to the passed argument and clear the m variable
                x <<- y
                m <<- NULL
        }
        get <- function() x #defines the function set which returns the matrix x
        setinv <- function(solve) m <<- solve #defines the function setinv which
                        #assigns the value of the solve function to m
        getinv <- function() m #defines the function getinv which returns the 
                             #variable m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) #returns as a list that can be called with the $ 
                        #operator the functions defined above
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv() #assigns the output of getinv to m
        if(!is.null(m)) { #checks if a an inverse was already saved to m
                message("getting cached data") 
                return(m) #in which case the cached inverse is returned
        }
        data <- x$get() #calls the get function to return the matrix x to data
        m <- solve(data, ...) #calculates the inverse of data to m
        x$setinv(m) #updates the cached inverse as m
        m #returns m, the inverse matrix
}
