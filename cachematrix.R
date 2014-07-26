## Cache inverse of a matrix

## makeCacheMatrix creates a special matrix object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  x.inv<-NULL
  set<-function(y){
    x<<-y
    x.inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) x.inv<<-inverse
  getinverse<-function() x.inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## If the matrix inverse has been calculated previously, cacheSolve function will computes the inverse of the special
## matrix returned by makeCacheMatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x.inv<-x$getinverse()
  if(!is.null(x.inv)){
    message("getting cache inverse matrix")
    return(x.inv)
  }
  data<-x$get()
  x.inv<-solve(data,...)
  x$setinverse(x.inv)
  x.inv
}
