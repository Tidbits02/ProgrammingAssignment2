## The functions below serves the purpose of creating and caching an inverse of a Matrix

## The makeCacheMatrix function takes and stores a matrix in the value of X.
## The makeCacheMatrix then returns a list of functions that can be used to work with -
## the matrix stored in the value X. 
##
## FUNCTION LIST:
## The 'get' function returns the orginal matrix
##
## The 'set' function overwrites the old matrix stored in the value x with -
## the new matrix stored in the variable y. Also if cached_m is set it get's overwritten -
## with NULL.
##
## The 'getinverse' function retreives the inversed matrix stored in cached_m or -
## NULL if the value has yet to be cached.
##
## The 'setinverse' function stores the inversed matrix in to the variable cached_m

makeCacheMatrix <- function(x = matrix()) {
        cached_m <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                cached_m <<- NULL
        }
        getinverse <- function() cached_m
        setinverse <- function(inverse) cached_m <<- inverse
        list(get = get, set = set,
             getinverse = getinverse,
             setinverse = setinverse)
}

## The cacheSolve function, takes a function (the makeCacheMatrix function to be specifc)
## and calls the function getinverse() (Think of it as x.getinverse() if 
## it was a method) inside of 'x', and stores the value in cached_m. If the value is 
## NOT 'NULL' it displays the message("getting cached data") and returns cached_m, then 
## exit's the program. If the value IS 'NULL' then it calls the get() function provided
## by x and stores the value (the matrix) in data. Then solve(data) is called and returns
## the inverse matrix and stores it in cached_m. Then the setinverse function is called
## and caches the inverse matrix globally in cached_m, then it returns cached_m, 
## and exits. cached_m stores the cached inverse matrix until x$set() is called.

cacheSolve <- function(x, ...) {
        cached_m <- x$getinverse()
        if(!is.null(cached_m)) {
                message("getting cached data")
                return(cached_m)
        }
        data <- x$get()
        cached_m <- solve(data, ...)
        x$setinverse(cached_m)
        cached_m
}

#EXAMPLES:

#> matrix = makeCacheMatrix(matrix(c(5,6,7,8), nrow=2, ncol=2))
#> matrix$get()
#[,1] [,2]
#[1,]    5    7
#[2,]    6    8
#
#> cacheSolve(matrix)  
#[,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
#
#> matrix$getinverse()
#[,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
#
#> cacheSolve(matrix)
#getting cached data
#[,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
