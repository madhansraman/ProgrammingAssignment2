## makeCacheMatrix : gets matrix type of argument (here x) and inverts the matrix for caching


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## setInvert and getInvert are defined for inverting the matrix that is passed as argument to makeCacheMatrix
        setInvert <- function(solve) m <<- solve
        getInvert <- function() m
        list(set = set, get = get, setInvert = setInvert, getInvert = getInvert) ## to print the results
}


## cacheSolve checks for cache of the inverted matrix. If so it prints the message that "getting cached data" 
##else it inverts the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvert()
        if(!is.null(m)){ ## Chcek if the cache exists. i.e. m is not null
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvert(m)
        m
}
