## Coursera - R Programming
## Programming Assignment 2: Lexical Scoping

### Programming Assignment will take advantage of the scoping rules of 
### the R language and how they can be manipulated to preserve state 
### inside of an R object. 

## Create an object in R that return the matrix inverse
## using the cache 
makeCacheMatrix <- function(x = matrix()){
  
  ## initialize the temporal value
  inver <- NULL;
  
  ## set the value of the matrix
  set <- function(matriz) {
    x <<- matriz;
    inver <<- NULL;
  }
  
  ## get the value of the matrix
  get <- function(){ x; }
  
  ## set the value of inverse of the matrix
  setInverse <- function(cacheSolve ) inver <<- cacheSolve; 
  
  ## get the value of inverse of the matrix
  getInverse <- function(){ inver; }
  
  list(set = set, get=get, setInverse=setInverse, getInverse=getInverse);
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(mc, ...){
  ## get the inverse matrix save in makeCacheMatrix
  temp <- mc$getInverse();
  
  ## If the inverse has already been calculated
  ## (and the matrix has not changed), then the cachesolve should 
  ## retrieve the inverse from the cache and the function end.
  if(!is.null(temp)) {
    message("getting cached data");
    return(temp);
  }
  
  ## Computing the inverse of a square matrix with the solve function in R.
  ## solve(X) returns its inverse. 
  ## For this assignment, assume that the matrix supplied is always invertible.
  mc$setInverse( solve( mc$get()  ) );
  
  ## returns its inverse. 
  return( mc$getInverse() );
}

##
# x <- matrix(c(1, 3, 3, 1, 4, 3, 1, 3, 4),nrow=3, ncol=3, byrow = TRUE)   
# ma  <- makeCacheMatrix(x)
# cacheSolve(ma)

## To confirm that this matrix is the inverse, multiplying the 
## two matrices and confirming that we get the identity.
##
# cacheSolve(ma) %*% ma$get()  

