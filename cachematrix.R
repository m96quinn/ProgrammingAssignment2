## These functions take a matrix as an input and return a list
## of functions that cache the matrix and its inverse
## The second function calculates the inverse if it does not already exist

## makeCacheMatrix returns a list that contains methods to 
## set and get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	Inv <- NULL
	set <- function(y) {
                x <<- y
                Inv <<- NULL
      }
	get <- function() x
	setInv <- function(inverse) Inv <<- inverse
	getInv <- function() Inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## cacheSolve checks to see if the inverse of a matrix has been
## cached yet. If so it returs it. If not it will calculate the inverse
## of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached matrix inverse")
                return(Inv)
        }
        data <- x$get()
	  Inv <- solve(data, ...)
        x$setInv(Inv)
        Inv
}
