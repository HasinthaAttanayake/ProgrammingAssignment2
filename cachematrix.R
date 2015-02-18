## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets the value of a matrix, gets the value of a matrix, sets the inverted matrix, gets the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL #initialize Matrix inverse as NULL 
    
    #input of y sets x for all functions within makeCacheMatrix
    setmatrix <- function(y){ 
      x <<- y   
      Inv <<- NULL 
    }
    getmatrix <- function() {x} #calls x
    setinv <- function(solve) {Inv <<-solve(x)} #Solves inverse of matirx x
    getinv <- function() {Inv}
    list(setmatrix=setmatrix,getmatrix=getmatrix,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
