## to cache matrix inverse


## creates special "matrix" object that caches its inverse;
## a list that contains 4 functions: set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)

}



## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)

	## Return a matrix that is the inverse of 'x'
	inv
}
