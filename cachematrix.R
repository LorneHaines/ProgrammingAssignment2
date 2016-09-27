####################################################################
# Filename: cachematrix.R
# Date: 9/27/2016
# Purpose:FUnctions work together to store the inverse of a matrix, so 
# that if it is called again it is pulled from the parent directory
# The first time the inverse is called it is taken an stored in parent directory
####################################################################

## makes a matrix that can cache its inverse. Makes a list of setters and getters
## that cacheSolve() will use to find the inverse and store in the parend directory

makeCacheMatrix <- function(x = matrix()) {
    # sets the inverse to null in the global environment
    parent_inv <- NULL
    # creates function for setting a value for the matrix in the parent environment
    # also parent_inv is given a null value
    set <- function(a) {
        x <<- a
        parent_inv <<- NULL
    }
    
    # creates function to get matrix from parent environment
    get <- function() x
    
    # creates function to set inverse in parent environment
    setinv <- function(inverse) parent_inv <<- inverse
    
    # creates function to get inverse from the parent environment
    getinv <- function() parent_inv
    
    # puts function sin a list with names, so that $ operator can be used
    list(set = set, get= get, setinv=setinv, getinv=getinv)
    
}


## finds the inverse of a square matrix from a matrix made with the 
## makeCacheMatrix function. If the inverse is already calculated
## returns cached value
cacheSolve <- function(x, ...) {
    # gets inverse if in parent envi otherwise returns null
    m_matrix <- x$getinv()
    
    # if there is inverse in the parent environment
    # returns it
    if(!is.null(m_matrix)){
        message("getting chached data")
        return(m_matrix)
    }
    
    # if inverse was not in parent envi gets matrix info of
    # matrix previously made with makeCacheMatrix()
    data <- x$get()
    
    # calculates and sets the inverse in the parent environment
    inv <- solve(data, ...)
    x$setinv(inv)
    
    # returns the inverse
    inv
}