## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        
        # a. set the value of the matrix
        # b. get the value of the matrix
        
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        # c. set the value of inverse of the matrix
        # d. get the value of inverse of the matrix
        
        setinversa <- function(inversaT) inversa <<- inversaT
        getinversa <- function() inversa
        list(set=set, get=get, setinversa=setinversa, getinversa=getinversa)
}




## Write a short comment describing this function


# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa <- x$getinversa()
        if(!is.null(inversa)) {
                message("...obteniendo datos de cache.")
                return(inversa)
        }
        datos <- x$get()
        inversa <- solve(datos)
        x$setinversa(inversa)
        inversa
}

