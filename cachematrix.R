## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##There are functions makeCacheMatrix
##makeCacheMatrix consist of set, get, setinv, getinv
##library(MASS) is used to calculate invers for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x
  }
  list(set = set, get= get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {    ##gets cache data
  inv <- x$getinv()
  if(!is.null(inv)){                ##checking whether inverse is null
    message("getting cache data!")
    return(inv)                     ##returns inverse value
  }
  data <- x$get()
  inv<- solve(data, ...)            ##calculates invers value
  x$setinv(inv)
  inv
}
