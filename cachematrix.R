# This is an exercise of the R Programming course, Programming Assignment 2
### This files contains 2 functions- first, makeCacheMatrix to create a "special" matrix 
### to be used inverse a matrix ans save to the cache. And the scond function is 
### cacheSolve used to solve this "special" matrix.

#The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse:
###set the value of the matrix
###get the value of the matrix
###set the value of the inverse
###get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # create the cache variable
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x # to get matrix
        setinverse_matrix <- function(inverse) m <<- inverse # here we store inverse in cache variable
        getinverse_matrix <- function() m # retriev the cached inverse
        list(set = set, get = get, # Return 
             setinverse_matrix = setinverse_matrix,
             getinverse_matrix = getinverse_matrix)
}



### CacheSolve determines if makeCacheMatrix already have cached results 
### before executing solve function.
### After, computes the inverse of the special "matrix" returned by 
### makeCacheMatrix above. If the inverse has already been calculated (and 
### the matrix has not changed), then the cachesolve should retrieve the 
### inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse_matrix()
        if(!is.null(m)) { # if it was calculated earlier
                message("getting cached data")
                return(m)
        }
        data <- x$get() # take R matrix out of cachable matrix
        m <- solve(data, ...) # and calculate its inverse
        x$setinverse_matrix(m)
        m #return the result
        
}
