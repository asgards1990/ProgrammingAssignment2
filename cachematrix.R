## Put comments here that give an overall description of what your
## functions do

## Create a class consisting of a list of methods to get and set 
## the inherent matrix, and to set and get the inverse of it.
## Notice that attributes of the object is not accessible directly
## in other words, x$inv will return NULL systematically.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv<- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Return the inverse of the matrix stocked by x an object
## of the instance makeCacheMatrix and store it in x.
## If an existing stored inverse already exists, no
## calculation will be made, the stored inverse will
## be returned and a courtesey message as below.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
