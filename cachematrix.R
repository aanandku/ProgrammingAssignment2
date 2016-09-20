
#Am creating a matrix object that can cache its inverse
## first I SET the value of the matrix,  GET the value of the matrix then SET the value of the inverse matrix and GET the value
## the list() does what I described in the line above


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
         x<<-y
         inv<<-NULL
}
get <-function() x
setmatrix<-function(solve) inv <<- solve
getmatrix<-function() inv
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

#calculates the inverse of the "matrix" created above
#checks to see whether inverse has been calculated, if it hasn't it goes ahead and calculates it
cacheSolve <- function(x, ...) {

    m <- x$getInverse()
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}
