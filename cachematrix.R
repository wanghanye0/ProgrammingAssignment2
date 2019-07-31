## Matrix iversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## the cades below achieve the functions that storing a matrix and caching its inverse.
## this function creates a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set<- function(y){
        x <<-y
        inv<<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv<<-inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## this function compute the inverse of the created "matrix". if the inverse has been calculated already, it would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
    if(!is.null(inv)){
        massage("getting cached data")
        return(inv)
    }
    data<- x$get()
    inv <-solve(data,...)
    x$setInverse(inv)
    inv
}
