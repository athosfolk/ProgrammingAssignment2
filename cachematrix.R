## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        minverse <- NULL
        set <- function(y){
                x <<- y
                minverse <<-NULL
        }
        get <- function() x
        setInverse <- function(inverse) minverse <<- inverse
        getInverse <- function() minverse
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
        
                
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If it has already been calculated it retrieves it from the cache 
cacheSolve <- function(x, ...) {
      minverse<- x$getInverse()
      if(!is.null(minverse)){
              message("getting cached inverse")
              return(minverse)
              
      }
      
        matr<- x$get()
        minverse<- solve(matr, ...)              
        x$setInverse(minverse)
        minverse
}
