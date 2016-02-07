## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a list of 4 functions:
## (1) to set the value of the matrix; (2) to get the matrix; 
## (3) to set the inverse, and (4) to get the inverse. 

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInv <- function(inv) m <<- inv
      getInv <- function() m
      list(set=set, get=get, setInv=setInv, getInv=getInv)
      
}


## The cacheSolve function checkes to see if the inverse (m) has been computed.
## The if mis NOT NULL, m is returned from the cache.  Otherwise, the data 
## is obtained via theget function, and m is calculated and returned

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInv()
      if(!is.null(m)) {
            message("getting cached data.")
            return(m)
      }
      data <- x$get()   ## gets data
      m <- solve(data)  ## creates inverse of matrix
      message("computing inverse.")
      x$setInv(m)       ## 
      m                 ## returns inverse 
      
}

## example usage below:
# > x = rbind(c(1,-1,0.5),c(-0.5,-1,1),c(0.75,-0.75,0.5))
# > x
# [,1]  [,2] [,3]
# [1,]  1.00 -1.00  0.5
# [2,] -0.50 -1.00  1.0
# [3,]  0.75 -0.75  0.5
# > m = makeCacheMatrix(x)
# > m$get()
#       [,1]  [,2] [,3]
# [1,]  1.00 -1.00  0.5
# [2,] -0.50 -1.00  1.0
# [3,]  0.75 -0.75  0.5
# > cacheSolve(m)
# computing inverse.
#           [,1]       [,2]     [,3]
# [1,] -1.333333 -0.6666667 2.666667
# [2,] -5.333333 -0.6666667 6.666667
# [3,] -6.000000  0.0000000 8.000000
# > cacheSolve(m)
# getting cached data.
#           [,1]       [,2]     [,3]
# [1,] -1.333333 -0.6666667 2.666667
# [2,] -5.333333 -0.6666667 6.666667
# [3,] -6.000000  0.0000000 8.000000
