### This combination of functions creates the inverse of 
## a square invertible matrix and caches the original and inverse in another environment.
## In the event a user attempts to use the inverse of a matrix, the program retrieves the cached
## inverse if already created instead of recreating a new one.


## The makeCacheMatrix function simply sets or gets a matrix and its inverse that are cached
## in an alternate environment.  No computations occur in this function.  This function 
## creates four functions- set,get,setinverse,getinverse- and outputs a list
## of these functions. The set function
## assigns the input matrix (y) to x and sets m to null.
## x and m are available to the other functions within makeCacheMatrix because of << assignment
## operator.  The get function simply retrieves x or the input matrix.
## The set inverse function by use of << assigns m to the function input (inverse) and makes
## m available to the other functions within makeCacheMatrix.  The set function does not
## create the inverse..this is done in the cacheSolve function.  It takes the inverse created in that function and
## assigns it to m.  The getinverse function retrieves m or the inverse matrix.  The x and m
## are the two objects- the cached matrix and inverse respectively- that are stored in this
## alternate environment and the four functions set or get them.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function takes the list created in makeCacheMatrix as input and uses the list
## objects.  If the output from the list object, the get inverse function, is null which will happen
## on first call, the solve function is called to create the inverse of a matrix.  
## The get function from the makeCacheMatrix list is called which gets the matrix x cached.  The inverse is assigned to m
## and is cached by setinverse function of the makeCacheMatrix list.  If the m is not null
## then a message and m is returned.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m

}
