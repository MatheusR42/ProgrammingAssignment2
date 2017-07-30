# Create a "special matrix", wich is really a list
# the "special matrix" has the the following functions:
# set, get, setInverse and getInverse
# it also caches the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL

    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function(){
    	x
    } 
    
    setInverse <- function(inverse){
    	inverseMatrix <<- inverse
    }

    getInverse <- function(){
    	inverseMatrix
    } 

    list(set = set, 
    	 get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# @param x is a "special matrix" made by makeCacheMatrix function
# Return a matrix that is the inverse of the parameter
cacheSolve <- function(x) {
	inverse <- x$getInverse()

    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    data <- x$get()
	inverse <- solve(data)
	
	x$setInverse(inverse)
	inverse
}

tediousMatrix <- matrix(c(3, -7, 5, 2), 2, 2)
specialMatrix <- makeCacheMatrix()
specialMatrix$set(tediousMatrix)
cacheSolve(specialMatrix)