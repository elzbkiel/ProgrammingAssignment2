## ### Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. This assignment is to write a pair of functions that
## cache the inverse of a matrix.

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                ##assign objects in another environment
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                inv <<- inverse  ## cache
        }
        getInverse <- function() {
                inv
        }
        
        ## list of functions to be identified by names, e.g. list$getInverse
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                ## Get from cache
                message("Getting cached data")
        }
        else {
                ## Calculate it first time
                m <- x$get()
                inv <- solve(m)
                x$setInverse(inv)
        }
        return(inv)
}

## How I tested it:

## Make a matrix by calling makeCacheMatrix:
## mymatrix <- makeCacheMatrix(matrix(1:4, 2, 2))

## Print this matrix:
## mymatrix$get()

## Inverse is not calculated yet (returns NULL):
## mymatrix$getInverse()

## Calculate Inverse using cacheSolve (returns inverse of the original matrix):
## cacheSolve(mymatrix)

## The second time, it returns it from cache (see message):
## cacheSolve(mymatrix)

## Function getInverse from makeCacheMatrix returns now the inverse:
## mymatrix$getInverse()

## Set another matrix as input:
## mymatrix$set(matrix(5:13, 3, 3))

## Print this matrix:
## mymatrix$get()

## Calculate inverse:
## cacheSolve(mymatrix)

## Verify that the solved matrix is inverse (returns determinant = 1):
## det(mymatrix$get() %*% mymatrix$getInverse())

##