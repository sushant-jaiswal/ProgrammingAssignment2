
# function makeCacheMatrix creates and returns a list of functions used by
# another function cacheSolve to get or set the inverted matrix in cache
# set() - set the value of the matrix
# get() the value of the matrix
# setInverse() - set the value of inverse of the matrix
# getInverse() - get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
    # initialize to NULL
    inv <- NULL
    
    # create the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get the value of matrix
    get <- function() x
    
    # invert the matrix and store in cache
    setInverse <- function(inverse) inv <<- inverse
    
    # get the inverted matrix from cache
    getInverse <- function() inv
    
    # return the list of functions
    list(set=set, get=get, setinverse=setInverse, getinverse=getInverse)
}

# function cacheSolve calcluates the inverse of the matrix 
# which is created in function makeCacheMatrix
cacheSolve <- function(x, ...) {
    
    # get the inverse of the matrix stored in cache
    inv <- x$getinverse()
    
    # if inverse of matrix is already calculated,
    # then return it from cache
    if(!is.null(inv)) {
        message("Getting data from cache!")
        return(inv)
    }
    
    # otherwise, create the inverted matrix
    data <- x$get()
    inv <- solve(data)
    
    # set inverted matrix in cache
    x$setinverse(inv)
    
    # show matrix
    message("Computing inverted matrix and getting data!")
    inv
}

# test run:
# > x = rbind(c(1, 2), c(3, 4))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# > cacheSolve(m)
# Computing inverted matrix and getting data!
#     [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# > cacheSolve(m)
# Getting data from cache!
#     [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5