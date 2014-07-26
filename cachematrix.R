## The makeCachematrix and Cachesolve functions will, respectively, allow the user
## to define and subsequently determine the inverse of a matrix.  The functions
## will incorporate cacheing to avoid the need for redundant calculations.

## The makeCacheMatrix function will allow the user to create a matrix, as well
## as establish the means to cache values that may subsequently be used by the
## cacheSolve function to determine the inverse of the matrix established through
## the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL  # inv_matrix is the variable that will contain the inverse matrix
        
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL  ##initializes inv_matrix so that the cacheSolve function
                ## can work properly
        } #end of the set function definition
        
        show_matrix <- function() x  ## provides contents of original matrix
        set_inverse <- function(solve) inv_matrix <<- solve   ## indicates that the solve function will be used
                ##to determine the inverse and initializes result.
        get_inverse <- function() inv_matrix ## provides contents of the inverted matrix
        
        list(set = set, show_matrix = show_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
                ## assigns values to the various entities defined within makeCacheMatrix
        
}    # end of makeCacheMatrix function definition


## The cacheSolve function will provide the user with the inverse of a matrix
## defined through the makeCacheMatrix function.  If the inverse of the matrix
## in question has been previously calculated, the cacheSolve function will display
## a cached solution to the user; otherwise, the inverse will be calculated using
## the Solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_matrix <- x$get_inverse()
        if(!is.null(inv_matrix)) {   ## As long as inv_matrix is not NULL, use the cached value.
                message("getting cached data")
                return(inv_matrix)
        } #end of if statement
        original_matrix <- x$show_matrix()  ## determine configuration and values of matrix 
                ## established through makeCacheMatrix function
        inv_matrix <- solve(original_matrix)  ## Use the "Solve" function to determine the
                ## inverse of the matrix established through the makeCacheMatrix function.
        x$set_inverse(inv_matrix)  ## Valuate "set_inverse" in the parameters of x so that
                ## the inverse matrix will not need to be calculated again.
        
        inv_matrix  ## display the inverse matrix
        
}  # end of cacheSolve function definition
