## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix. 
## rather than compute it repeatedly this pair of functions creates a cache of the inverse of a matrix.

## This function creates a special matrx object that caches its inverse.If the inverse
## was already calculated it will return the the calculated inverse, if not it will
## calculate and cache it. 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## This function verifies it the reverse of matrix was computed, if not will compute.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
