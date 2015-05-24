## Here are two functions - (1) makeCacheMatrix and (2) cacheSolve - to cache
## the inverse of a square invertible matrix. 


## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

### x is the variable to store the matrix
### inv is the variable to store its inverse

#### The function returns a list of 4 functions  - set, get, setinv, getinv -
#### to manipulate x and inv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                    
        set <- function(y) {           #set for setting your matrix to var x. 
                x <<- y                #inv initialized to NULL everytime a
                inv <<- NULL           # new matrix is set to x
        }                              
        get <- function() {            #get for retrieving the matrix in x
                x
        }
        setinv <- function(inverse) {  #setinv for setting the inv variable 
                inv <<- inverse        # preferably to cache inverse of matrix
        }
        getinv <- function() {         #getinv for retrieving the value
                inv                    # stored in inv
        } 
        list(set = set, get = get,     #list of 4 functions above is returned
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve function retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()   # From special matrix object, value of inv is retrieved            
        if(!is.null(inv)) { # If inv is not NULL then no need to compute inverse
                message("getting cached data") 
                return(inv) # Simply return the cached value of inv
        }
        
        # If inv is NULL then inverse matrix is computed below using 'solve' function
        data <- x$get()     
        inv <- solve(data, ...)
        x$setinv(inv)       # The inverse matrix computed is cached here,
        inv                 # and returned here.
}
