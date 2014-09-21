## Two functions to cache a matrix, 
## and one to retrieve or calculate as needed

## This function creates a special matrix 
##that can cache its inverse if necessary

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL #Set up blank 
        
        #Set Matrix
        set <- function (matrix) { 
                x <<- matrix
                inv <<- NULL
        }
        
        ##Get Matrix
        get <- function () { x }
        
        ##Set Inverse
        setInv <- function(inv) {
                inv <<- inv
        }
        
        ##Get Inverse
        getInv <- function() {
                inv
        }
        
        ##Return all methods
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
        
}

## Compute the inverse of the special matrix 
##from the function above

cacheSolve <- function(x, ...) {
        m <- x$getInv()  #return inverse of 'x'
        
        #return inv if already set
        if(!is.null(m)) {
                print("getting cached matrix")
                return(m)
        }
        
        data <- x$get()  #get matrix
        
        m <- solve(data) %*% data  ##calculate inverse
        
        x$setInv(m)  #Set inverse
        
        m  #Print
}


