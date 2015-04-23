## Put comments here that give an overall description of what your
## functions do
## functions developed to store and retrieve inverse matrix via cache
## take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object
## how to use:
## matriz<-makeCacheMatrix()
## matriz$set(matrix(1:4,2,2)
## cacheSolve(matriz) - several times to get cached data

## Write a short comment describing this function
## this function creates an object that has methods to set and get values for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## this function returns the inverse of a matrix, calculating it by the solve function or getting it from cache if
## it has been previously calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
