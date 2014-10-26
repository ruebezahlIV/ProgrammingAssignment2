##
##
## Programming assignment 2, week 3 Intoduction to R 
##
## Functions to caclulate and cache the inverse of a given matrix
##
##


## Function which creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
           
    ## reset inverted matrix
    im <- NULL
    
    ## Function which gets the values of the given matrix 
    getmat <- function() x      
    
    ## Caching the inverse of the matrix 
    setinvmat <- function(invmat) {   
        im <<- invmat
    }
       
    ## Function which sets the cached values of the matrix
    setmat <- function(y) {
        x <<- y 
        im <<- NULL
    }
    
    ## Function which gets the values of the inverse of the matrix 
    getinvmat <- function() im     
        
    ## return the list of functions above to make them available for other functions    
    list(getmat = getmat,
         setmat  = setmat,
         getinvmat  = getinvmat,
         setinvmat = setinvmat)
    
}   
 
## Function which computes the inverse of a matrix.
## If the inverse has already been calculated, it retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
    
    ## Call the function to retreive the inverted matrix
    mat <- x$getinvmat()
    
    ## If data is available return the cached data
    if(!is.null(mat)) {      
        message("Getting cached matrix data")
        return(mat)
    }
    
    ## If no cached data is available then get the original matrix
    matdata <- x$getmat()
   
    ## Calculating the inverse of the matrix x  
    invmatc <- solve(matdata) 
    
    ## Cache the result
    x$setinvmat(invmatc)
    
    ## Return the inverse matrix 
    return(invmatc)
    
}
