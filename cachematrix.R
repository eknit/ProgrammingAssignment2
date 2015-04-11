## A pair of functions that caches the inverse of a matrix

# Creates a list that contains functions to set the value of the original matrix ('x'), to get the value of the matrix x, to set the value of the inverse of the matrix ('m') and to get the value of the inverse of the matrix ('m')

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
	get <- function() x
	setmatrix <- function(matrix) m <<- matrix
	getmatrix <- function() m
	list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Calculates the inverse of the matrix 'x' that was created with the above function. If already calculated, it retrieves the inverse from the cache, else it calculates the inverse and sets the value to the inverse of the matrix cached by the setmatrix function

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
