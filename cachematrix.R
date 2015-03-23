## This file contains two functions. The first function stores an input 
## matrix and caches its inverse. The second function can check for a
## pre-existing inverse in the cache and place the inverse in the cache if
## one was not found. These functions will not work correctly if the input
## matrix is non-square or singular.

## makeCacheMatrix will return a list with 4 function elements (setting the 
## input matrix, getting the input matrix, setting the inverse of the matrix, 
## and getting the inverse of the matrix).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                       # creates an empty object "m"
    set <- function(y) {            # set will store the input matrix "x"
        x <<- y
        m <<- NULL
    }
    get <- function() x             # get will return the input matrix "x"
    setinverse <- function(x) m <<- solve(x)
         # setinverse will define "m" to be the inverse of "x"
    getinverse <- function() m      # getinverse returns the matrix "m"
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)   #returns a list of 4 functions
}


## cacheSolve checks to see if an inverse of an input matrix exists in the 
## cache, and returns the inverse from the cache if found or, if not found, 
## computes it and places it in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m))          #checks to see if inverse exists in cache
        {
        message("getting cached data")
        return(m)            #returns cached inverse if found
    }
    data <- x$get()          #creates "data" and sets as input matrix 
    m <- solve(data, ...)    #sets "m" to be inverse of "data"
    x$setinverse(m)          #runs "setinverse" on "m"
    m                        #returns "m", which will now be inverse of input
}
