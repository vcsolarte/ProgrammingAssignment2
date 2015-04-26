## Put comments here that give an overall description of what your
## functions do

## Make and set matrix, write functions to inverse matrix
makeCacheMatrix <- function(x = matrix()) 
{
     
     invmatrix <- NULL
     setmatrix <- function(y) 
     {
          x <<- y
          invmatrix <<- NULL
     }
     
     getmatrix <- function() x
     setinverse <- function(inverse) invmatrix <<- inverse
     getinverse <- function() invmatrix
     list(set=setmatrix, get=getmatrix, setinverse=setinverse, getinverse=getinverse)
     
}

## Return inverse matrix, state if pulled cached or not
cacheSolve <- function(x, ...)  
     ## Return a matrix that is the inverse of 'x'
{
     
     invmatrix <- x$getinverse()
     if(!is.null(invmatrix)) 
     {
          message("getting cached data.")
          return(invmatrix)
     }
     
     matrix <- x$get()
     invmatrix <- solve(matrix)
     x$setinverse(invmatrix)
     
     invmatrix
     
}
