## 

##R session:
## lexical scope programming assignment 2

## a pair of function that cache the inverse of matrix
## create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
## function with matrix as argument   
## initialize the inverse property   
   s <- NULL
## function to set the  matrix
   set <- function(y){
      x <<- y ## assignment the y value to parent environment
      s <<- NULL ## assign null to s in parent environment
   }
## function to get the matrix
   get <- function() x
## function to set the solve matrix from other environment
   setsolve <- function(solve) s <<- solve
## function to get the solve matrix
   getsolve <- function() s
   list(set = set,get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}
## create a special solve function which test if there a cache inverse
## if not doing computional to solve matrix
## always return the solve matrix
## we need a special matrix doing by makeCacheMatrix to run
cachesolve <- function(x,...){
## assign the value of x$getsolve to s
   s <- x$getsolve()
## chich if s is contain solve then message and return solve without computation
   if(!is.null(s)){
      message("getting cached data")
      return(s)
   }
## assign matrix in get function to data   
   data <- x$get()
## solve the matrix and save the result in s   
   s <- solve(data,...)
## caches the solve matrix in set solve environment location   
   x$setsolve(s)
##  return the solve matrix to the parent environment 
   s
}
