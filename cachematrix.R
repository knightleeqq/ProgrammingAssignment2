## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

## set the value of the Matrix
## get the value of the Matrix
## set the value of the Matrix
## get the value of the Matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## Write a short comment describing this function
## The following function calculates the inversion of the special "matrix" created with the above function. However, it first checks to see if the inversion has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the inversion of the data and sets the value of the inversion in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
 
# > source("cachematrix.R")
# > cc<-makeCacheMatrix(c)
# > cc$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(cc)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > cacheSolve(cc)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
