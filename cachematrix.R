## Programming assignment 2


makeCacheMatrix <- function(x = matrix()) {
        ## This function makes a special 'matrix' object, which is really a 
        ## list, containing functions that sets and gets the value of the
        ## matrix 'x' and its inverse 'i'
        
        i <- NULL       ## stores the inverse of the matrix
                        ## now it's NULL (we have nothing to store yet!)
        
        set <- function(y) {    ## sets the value of the matrix into the list
                x <<- y
                i <<- NULL      ## it's still NULL because the inverse is not
                                ## calculated at this time
        }
        
        get <- function() x     ## returns the value of the original matrix, 
                                ## which is stored in the list
        
        setInv <- function(inv) i <<- inv       ## sets the value of the  
                                                ## inverse of the matrix (inv)
        
        getInv <- function() i  ## gets the value of the inverse matrix, which 
                                ## is stored in the list
        
        list(set = set, get = get,    ## the list that stores the data
             setInv = setInv,
             getInv = getInv)

}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. 
        
        ## Its input must be an object (a special 'matrix')produced by the 
        ## function makeCacheMatrix(x)
        
        
        ## First, checks if the value has been already calculated and stored
        ## into the special 'matrix'. If the value is there, the function will
        ## return that value.
        
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If the inverse matrix has not been calculated before, the function
        ## will then get the value of the original matrix, calculate the 
        ## inverse (using the solve(x) function) and store the values in the 
        ## respective spaces in the special 'matrix' object.
        
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
           
}
