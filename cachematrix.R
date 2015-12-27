makeCacheMatrix <- function(x = matrix()) {
  #results of inverse function will be stored in invm
  invm <- NULL
  #the set function will allow for a 
  #free variable outside of the function, also clearing the cache for inversion
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  #will return the input matrix
  get <- function() x
  #will set the inverse matrix
  setInverse <- function(solve) invm <<- solve 
  #will return the inverse matrix
  getInverse <- function() invm
  #these are the available calls for the function
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
  #Set inv to the inverse of x
  inv <- x$getInverse()
  #if x$getInverse() is null then the below statement will not run
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if null, then the below will get the setted matrix
  data <- x$get()
  #will solve for the inverse of x
  inv <- solve(data)
  #sets the inverse of x in the list
  x$setInverse(inv)
  #returns the results
  inv
}
#run test 
test <- makeCacheMatrix()
test$set(matrix(runif(16,1,150),4,4)) 
#returns the inversion in the working environment
cacheSolve(test)
#[,1]       [,2]        [,3]         [,4]
#[1,] -0.01891055 -0.1076539  0.07109003 -0.009633979
#[2,]  0.06540761  0.4207159 -0.23544921  0.021914651
#[3,] -0.14561681 -0.8276939  0.46512884 -0.008032818
#[4,]  0.09973687  0.5101358 -0.30122790  0.015278550
#returns the inverse matrix from cache
test$getInverse()
#[,1]       [,2]        [,3]         [,4]
#[1,] -0.01891055 -0.1076539  0.07109003 -0.009633979
#[2,]  0.06540761  0.4207159 -0.23544921  0.021914651
#[3,] -0.14561681 -0.8276939  0.46512884 -0.008032818
#[4,]  0.09973687  0.5101358 -0.30122790  0.015278550

