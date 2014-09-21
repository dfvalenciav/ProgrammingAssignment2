## First the function takes a set of numbers (c). Next, it calculates the length
## of c, next, it takes the half of length (b) which is the number of nrow and
## ncol. Next it takes the this numbers and made a square matrix. Finally,
## it calculates the inverse matrix 

makeCacheMatrix <- function(c) {
    a<-length(c)
    b<-a/2
    d<-round(b, 0)
    x<-matrix(c,nrow=d,ncol=d)
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
}


## this function checks if the inverse of matrix have been calculated above. If 
## it have not been calculated, this function do it and finally it returns the 
## inverse of matrix

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m   
    ## Return a matrix that is the inverse of 'x'
}