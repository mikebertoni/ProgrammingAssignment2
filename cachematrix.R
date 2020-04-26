## Put comments here that give an overall description of what your
## functions do

## This function creates a list that stores four functions (set, get, setinverse and getinverse), the matrix x, and an inverse matrix (NULL by default).
## The result of this function is meant to be fed to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL #sets the inverted matrix to null at each new call
    set <- function(y) { #this function stores the matrix s and resets its inverse
        x <<- y #y is passed from the parent function x
        inverse <<- NULL
    }
    get <- function() x #when called, this function returns the original matrix x
    setinverse <- function(z) inverse <<- z #this function stores the inverted matrix (NULL if never calculated)
    getinverse <- function() inverse #when called, this function returns the inverted matrix (NULL if never calculated)
    list(set = set, get = get,          #creates a list of four functions, the matrix x, and the inverted matrix if exists, to be fed to cacheSolve
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function does the actual inversion. First it retrieves the inverted matrix from getinverse. If it is not null,
## it returns the cached value. If it is null, it calculates it and stores it in the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse() #retrieves the inverse matrix stored by getinverse
    if(!is.null(inv)) { #if inv is not null, it has been already calculated by cacheSolve
        message("getting cached data")
        return(inv) #returns the cached inverted matrix
    }
    data <- x$get() #if inv is NULL, it must be calculated. First it gets the matrix...
    inv <- solve(data, ...) #...then it inverts it 
    x$setinverse(inv) #the result is passed to the setinverse function, for future retrieval
    message("this is a new calculation") #just to make sure the function works as intended.
    inv #prints the result
}
