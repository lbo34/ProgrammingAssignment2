## These are the functions for week 3 programming assignment
## functions inverse a matrix but first check if the inverse has already been computed.

## This functions just chaches the inverse of the matrix in the im variable.

makeCacheMatrix <- function(x = matrix()) {
  im<-NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setim<-function(inversed) im<<-inversed
  getim<-function() im
  list(set=set, get=get, setim=setim, getim=getim)
}


## this function checks if the result of getim is null before making the inversion, otherwise takes cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im<-x$getim()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  mymat <- x$get()
  im <- solve(mymat)
  x$setim(im)
  im
}
