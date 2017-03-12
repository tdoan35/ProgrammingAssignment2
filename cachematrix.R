## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix object that can cache its inverse

## Write a short comment describing this function

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	## Setting the value of the matrix
	m <- NULL 
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	## Getting the value of the matrix
	get <- function() x
	## Setting the value of the inverse of the matrix
	setinverse <- function(inverse) m <<- inverse
	## Getting the value of the inverse of the matrix
	getinverse <- function () m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse
## from the cache 

## Computing the inverse of a square matrix can be done with the solve
## function in R. Assume the matrix supplied is alwasy invertable (square)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
