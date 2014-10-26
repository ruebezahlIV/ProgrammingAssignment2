##
##
## Programming assignment 2, week 3 Intoduction to R 
##
## Functions to caclulate and cache the inverse of a given matrix
##
##
## Example: 
##


## Function which creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
           
    ## Function which gets the values of the given matrix 
    getmat <- function() x      
       
    ## Calculating the inverse of the matrix x  
    invmat <- solve(getmat) 
    
    ## Function which sets the cached values of the inverse of the matrix
    setinvmat <- function(y) {
        x <<- y 
    }
    
    ## Function which gets the values of the inverse of the matrix 
    getinvmat <- function() invmat      
        
    ## return the list of functions above to make them available for other functions    
    list(fgetmat = getmat,
         getinvmat  = getinvmat,
         setinvmat  = setinvmat)
    
}   
 
## Function which computes the inverse of a matrix.
## If the inverse has already been calculated, it retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
    
    ## Call the function getmatrix to retreive the data
    mat <- x$getinvmat()
    
    ## If data is available return the cached data
    if(!is.null(mat)) {
        message("getting cached matix data")
        return(mat)
    }
    
    ## If no cached data is available then get the original matrix
    matdata <- x$getmat()
   
    ## Calculating the inverse of the matrix x  
    invmatc <- solve(matdata) 
    
    ## Cache the result
    x$setinvmat(invmat)
    
    ## Return the inverse matrix 
    return(invmatc)
    
}
