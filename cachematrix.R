## Put comments here that give an overall description of what your
## functions do

# 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        #Description/ Output: This function creates a special "matrix" object
        #that can cache its inverse
        
        #Input: x as a square, invertible matrix (Input from cacheSolve)
        #1.  set the value of the matrix
        #2.  get the value of the matrix
        #3.  set the value of the inverse
        #4.  get the value of the inverse
        
        
        #Initiatve variable
        inverse_i <- NULL
        
        #1-4
        set <- function(y) {
                x <<- y
                inverse_i <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inverse_i <<- inverse
        getinverse <- function() inverse_i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}





## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Description: Return a matrix that is the inverse of 'x'
        #Input: x = output from function makeCacheMatrix()
        #Output: Inverse of x => input for makeCacheMatrix()
        
        #Check if the inverse of x has already been cached/ calculated
        inverse_i=x$getinverse()
        
        if(!is.null(inverse_i)) {
                message("getting cached data...")
                return(inverse_i)
        }
        
        #In case the inverse of x has not been calculated/ cached yet, calculate the inverse
        inverse_data.data = x$get()
        inverse_i=solve(inverse_data.data, ...)
        
        #Set and return the inverse
        x$setinverse(inverse_i)
        
        return (inverse_i)
}
