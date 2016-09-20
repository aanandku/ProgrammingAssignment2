
#Am creating a matrix object that can cache its inverse
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

#Creating a function that computes the inverse of the matrix created above

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
