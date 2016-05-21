## Jorge Monsegny 2016
## This is the second assignment of the course R
## Programming from the JHU-Coursera Data Science
## Specialization.
## The objective is to write two functions that 
## use R lexical scoping to avoid the matrix inverse
## calculation by returning a cached result whenever 
## is possible. 

## This function receives a matrix and returns
## a list with setter and getter functions for
## the matrix and its inverse.

makeCacheMatrix <- function( x = matrix() ) {
    invx <- NULL
    #When setting a new matrix, inv es reset
    set <- function( y ) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinv <- function( inv ) invx <<- inv
    getinv <- function() invx
    #List with setter and getter functions
    list( set = set, get = get, 
          setinv = setinv,
          getinv = getinv )
     
}


## This function receives a list returned by
## the makeCacheMatrix function and returns
## the inverse of the matrix only is it was
## not previously calculated.

cacheSolve <- function( x, ... ) {
    invx <- x$getinv()
    #If cached inverse exists
    if( !is.null( invx ) ) {
        message("getting cashed inverse")
        return( invx )
    }
    #Calculate inverse if it doesn't exist
    data <- x$get()
    invx <- solve( data, ... ) #solve calculates matrix inverse
    x$setinv( invx ) #cash new inverse
    invx
}
