## Put comments here that give an overall description of what your
## functions do 
##  This function creates a special "matrix" object

## Write a short comment describing this function
## This function creates 4 function
## set : set the  matrix ,get : get the matrix
## setinverse : set the inverse of matrix  , getinverse: get the inverse of matrix

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


## Write a short comment describing this function

## this function first checks wheather the inverse of matrix is calculated
## if not calcultes the inverse and sets it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

