## cachematrix.R : this package includes 2 functions.
## functions:
## 1) makeCacheMatrix: holds the original matrix and cached inverse matrix
## 2) cacheSolve : this function checks is inverse matrix is already calculated and cached 
##                 if not calculates and caches it. 

## function: makeCacheMatrix 
## This is a function to cache inverse matrix
## set - sets the value of the matrix 
## get - gets the value of the matrix
## setinverse - sets the inverse matrix
## getinverse - gets the inverse matrix
## all of these setter and getter function are put into a special vector; a list

makeCacheMatrix <- function(x = matrix()) {
        ## Inverse matrix cache
        i  <- NULL
	set  <- function(y){
	x <<- y
	i <<- NULL 
	}
	get  <- function() x
	setinverse  <- function(inverse) i  <<- inverse
	getinverse  <- function() i
	list(set= set, get = get, 
	setinverse = setinverse, 
	getinverse = getinverse)
}

## function: cacheSolve
## This function use checks for cached inverse matrix and returns it else calculates and caches it.
## Inverse matrix is calculated using r function "solve"   

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i  <- x$getinverse()
	if (!is.null(i)){
	message("retrieved cached inverse matrix")
	return(i)
	}
	data  <- x$get()
	i  <- solve(data, ...)
	x$setinverse(i)
	i
}
