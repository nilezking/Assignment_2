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


cache_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
cacheSolve(cache_matrix)

