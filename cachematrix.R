## I just modified makeVector and cacheVector. makeCachedMatrix take a matrix as argument
## and get it stored in memory.
## CacheSolve takes the same argument from makeCacheMatrix and calculate the inverse of the matrix,
## if it has not been calculated before.

## The function below produce a list of function and two matrix (x, inv).
## set function change the values of x while get gets the content of x.
## getinverse and setinverse are similar to get and set, but they work with inv.
##inv is set as generic matrix so it can clean other values when x is set again.

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        set <- function(y = matrix() ){
                x <<- y
                inv <- matrix()
        }
        get <- function() x
        setinverse <- function(solve) inv<<- solve
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## CacheSolve calculates the inverse matrix of x. The if instruction control the content
## of inv. If in inv is stored the inverse of x it return it, otherwise cacheSove solve x.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.na(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
