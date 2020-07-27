#set input x as a matrix
#set solved value "s" as a null
#changed "mean" to "solve"

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)
}

#changed "mean" to "solve"
#changed "m" to "s"
cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("Inversed Matrix...")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
