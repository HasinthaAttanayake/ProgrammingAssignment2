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
    getmatrix <- function() x #calls x
    setinv <- function(Fun = solve) Inv <<- Fun(x) #Solves inverse of matirx x, with defualt argument as the 'solve' fucntion
    getinv <- function() Inv #calls value of inv
    list(setmatrix=setmatrix,getmatrix=getmatrix,setinv=setinv,getinv=getinv) #function is expressed as a list of functions. 
}


##This function computes the inverse of the special"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv <- x$getinv() #load inverse value from previous function
    #if statement checks for non null inverse values, if present it will return Inverse of matrix.
      if (!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
      }
    # if Inv is null then cacheSolve will take matrix loaded into makeCacheMatrix and run it through Solve.
    matdat <- x$getmatrix()
    Inv <- solve(matdat)
    x$setinv(Inv)
    Inv
}

#Testing Mechanism
#create a square matrix of n,n dimensions and store as variable e.g mat1
# m<-makeCacheMatrix
#run lists loading and computing data from functions in 'm'
# m$setmatrix(mat1)
# m$getmatrix()
# m$setinv()
# m$getinv()
#check catched data with cacheSolve
# cacheSolve(m)
#Output is 'getting cached data' + Inverse of Matrix.
#if m$setinv is not initialised in console then inverse is caluclated by cacheSolve
