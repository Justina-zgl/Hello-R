## Caching the Inverse of Matrix
## Functions below are applied for creating a "matricx" that cache its inverse.

## This function make a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <-Null
    set <-function(y){
      x<<-y
      i <<-Null
}
    get<-function()x
    setInverse<-function(inverse) i <<-inverse
    getInverse<-function() i
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)

## This function calculates the inverse of the "matrix" created by above function
## If the inverse has been calculated, it get the results from the cache and skip
## the computation. Otherwise, it calculate the inverse of Matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <-x$getInverse()
        if (!is.null(inv)){
            message("getting cached data")
            return(i)
        }
        data<-x$get()
        i<-inverse(data,...)
        x$setInverse(i)
        i
}