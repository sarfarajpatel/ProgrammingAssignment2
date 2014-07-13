## A pair of functions that calculates the inverse of matrix and if already calculated returns the cached value of the 
## inverse


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( mat= matrix() ) {

	## Initialize the inverse property
    inv <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            mat <<- matrix
            inv <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	mat
    }

    ## set method for inverse
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## get method
    getInverse <- function() {
        inv
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the matrix and if already calculated returns the cached value of the matrix
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    mat <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(mat) ) {
            message("getting cached matrix")
            return(mat)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    mat <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(mat)

    ## Return the matrix
    mat
}
