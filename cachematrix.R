##Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions are used to cache the inverse of a matrix.


## makeCacheMatrix()
## 	This function creates a special "matrix" object that can cache its inverse
## 	input x:		a square ivnvertible matrix
## 	output list:   	a list containing 4 functions to
## 	1.set 		set the value of the matrix 			
## 	2.get 		get the value of the matrix			
## 	3.setinverse	set the value of inverse of the matrix		
## 	4.getinverse	get the value of inverse of the matrix	
##	the list is used as input to cacheSolve()


makeCacheMatrix <- function(x = matrix()) {

        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinverse = function(inverse) inv <<- inverse 
        getinverse = function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve() 
## 	This function computes the inverse of the special "matrix" returned
## 	by makeCacheMatrix().If the inverse has already been calculated (and the matrix 
## 	has not changed), then the cachesolve retrieves the inverse from the cache.
## 	input x:		a list, output from makeCacheMatrix()
## 	output inv:		an inverse of orginal matrix, input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv = x$getinverse()
 
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data = x$get()
        inv = solve(data, ...)
        x$setinverse(inv)
        return(inv)
}
