## These functions are to explore lexical scoping as part 
## ofCoursera course in R Programming with JHU 

## This function, makeCacheMatrix() contains the following functions
##
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse
##   get the value of the inverse
##
## it returns a list of above functions

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function()  x 
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	# return list otherwise $ will not work
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

## This function, cacheSove() calculate the inverse of the matrix created with above function.
##
## It first checks if inverse is already calculated, if so it returns 
## the cached inverse, otherwise it calculates the inverse and caches
## the inverse via the "setinverse" function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting caches data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Testing
## Examples (ref: https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/)
## >    source(“cachematrix.R”)
## >    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## >    amatrix$get()         # Returns original matrix
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## >   cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >  amatrix$getinverse()  # Returns matrix inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >  cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >    amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
## >    cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
## [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
## >    amatrix$get()         # Returns matrix
## [,1] [,2]
## [1,]    0   99
## [2,]    5   66
## >    amatrix$getinverse()  # Returns matrix inverse
## [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0
