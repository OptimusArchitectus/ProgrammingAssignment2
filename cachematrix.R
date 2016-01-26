# The first function creates a list of functions to access/modify a matrix and it's inverse.
# The second function uses the solve function to produce the given matrix's (x) inverse 
# if it has not been previously computed. It will then pass the newly computed inverse to 
# the setSolve element of the makecacheMatrix function to be cached. 


# A short comment describing this function :D -- list of functions to set and get matrix and inverse. 
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) s <<- solve
        
        getsolve <- function() s
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


# A short comment describing this function :D -- sets inverse of matrix and passes value 
# to cache function if cache element is nul 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        data <- x$get()
        
        s <- solve(data, ...)
        
        x$setsolve(s)
        
        s
}
