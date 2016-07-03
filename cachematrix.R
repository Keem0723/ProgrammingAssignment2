## R Programming Homework Week 3
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
 
## Objective: Write the following functions
# 1.`makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.

# 2.`cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then 'cacheSolve` should retrieve the inverse from the cache.

## Note: 
# Computing the inverse of a square matrix can be done with the `solve` function in R. For example, if `X` is a square invertible matrix, then 'solve(X)` returns its inverse.

## makeCacheMatrix`: 
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
        matinv <- NULL
        set <- function(x) {
                mat <<- x
                matinv <<- NULL
        }
        get <- function() mat
        setinv <- function(solve) matinv <<- solve
        getinv <- function() matinv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## `cacheSolve`: 
# This function computes the inverse of the special "matrix" returned by `
# makeCacheMatrix` above. If the inverse has already been calculated (and the 
# matrix has not changed), then 'cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
        matinv <- mat$getinv()
        if (!is.null(matinv)) {
                message("Getting cached data")
                return(matinv)
        }
        data <- mat$get()
        matinv <- solve(data, ...)
        mat$setinv(matinv)
        matinv
}
