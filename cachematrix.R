## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function to set and return the cache of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     set_inv <- function(solve) inv <<- solve
     get_inv <- function() inv
     list( set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}


## cacheSolve is a function to invert a matrix and cache the results

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)){
             message("getting cached data")
             return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
}
