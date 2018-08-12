## makeCacheMatrix create a list of functions that can store and access
# a matrix and its inverse into a cache 

## This function creates functions to set and get data and the inverse 
# of that data into/from a specific cache

makeCacheMatrix <- function(storage = matrix()) {
  invers <- NULL
  setdata <- function(DataToStore) {
    storage <<- DataToStore
    invers <<- NULL
  }
  getdata <- function() storage
  setinv <- function(calculatedinverse) invers <<- calculatedinverse
  getinv <- function() invers
  list(setdata = setdata, getdata = getdata,
       setinv = setinv,
       getinv = getinv)
}


## This function checks to see if the inverse you want is already
# located in the cache to return to you.  If it hasn't been calculated
# it will calculate and store it for later before returning the inverse.

cacheSolve <- function(storedfunctions, ...) {
  invers <- storedfunctions$getinv()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  data <- storedfunctions$getdata()
  invers <- solve(data, ...)
  storedfunctions$setinv(invers)
  invers
}
