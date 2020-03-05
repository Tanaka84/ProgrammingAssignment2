## The functions create a matrix that is stored in the general enviroment, then it solves the inverse of the matrix
## and prints the result. If the inverse has already been calculated the functions just grab the cached data. 
## The inverse of the matrix remains cached until the matrix changes. 

## This function stores the values of ma and x to be used later, then creates four functions to act as setters and getters
## and finally coalesces them in a list.

makeCacheMatrix <- function(x = matrix()) {
      ma <- NULL
      set <- function(y) {
            x <<- y
            ma <<- NULL
      }
      get <- function() x
      setmatrix <- function(matrix) ma <<- matrix
      getmatrix <- function() ma
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}




## This function first calls the object "ma" (the inverse of the matrix) if the value is different from null it prints a
## messaged and the cached value. If it's null then it creates an object that contains the original matrix and uses the
## solve function to calculate the inverse. 

cacheSolve <- function(x, ...){
      ma <- x$getmatrix()
      if(!is.null(ma)) {
            message("getting cached data")
            return(ma)
      }
      data <- x$get()
      ma <- solve(data, ...)
      x$setmatrix(ma)
      ma
}

