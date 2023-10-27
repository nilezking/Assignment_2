## Two functions makeCacheMatrix and cacheSolve for efficient matrix inversion computations.

## makeCacheMatrix function creates a specialized matrix object that can 
## store both an original matrix and its inverse. It accepts an original matrix
## as an input and provides methods to set and retrieve the matrix and its inverse

makeCacheMatrix <- function(inputMatrix = matrix()) {
    cache <- NULL
    set <- function(y){
        inputMatrix <<- cache
        cache <<- NULL
    }
    get <- function() {
        inputMatrix
    } 
    setInverse <- function(inverse) {
        cache <<- inverse
    } 
    getInverse <- function() {
        cache
    }  
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    
}


## cacheSolve function computes and retrieves the inverse of a matrix 
## leveraging a cache mechanism to store and retrieve previously 
## computed inverses. It takes an inputMatrix as input, which should be created 
## using the makeCacheMatrix function, and returns the cached inverse if available, 
## or calculates, caches, and returns the inverse while providing informative messages.

cacheSolve <- function(inputMatrix) {
    cache <- inputMatrix$getInverse()
    if(!is.null(cache)){
        message("Cache is being fetched from memory")
        return(cache)
    }
    mat <- inputMatrix$get()
    cache <- solve(mat)
    inputMatrix$setInverse(cache)
    return(cache)
}


## Function calls for both functions above.

cache_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
cacheSolve(cache_matrix)
