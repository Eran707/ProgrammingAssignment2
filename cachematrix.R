## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function houses the cached inverse matrix
# mat refers to the matrix that is passed, both the normal and the inverse matrix can be stored here
# The <<- operator stores a local variable in the function which is cached

makeCacheMatrix <- function(x = matrix()) {
        inverse_mat <<-NULL
        
        set_mat <- function(){
                cached_mat <<-x
                inverse_mat <<- NULL
        }
        get_mat<- function()x
        set_inverse<- function(inverse) inverse_mat <<- inverse
        get_inverse<- function() inverse_mat
        list(set_mat = set_mat, get_mat = get_mat, set_inverse= set_inverse, get_inverse = get_inverse)
        
}

## Write a short comment describing this function
# This function returns the cached inverse (if it exists), otherwise calculates the inverses

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_mat <-x$get_inverse()
        if(!is.null(inverse_mat)){
                message("getting cached data")
                return(inverse_mat)
        }      
        data <- inverse_mat$get()
        inverse_mat <- solve(data)
        x$set_inverse(inverse_mat)
        inverse_mat
        
}