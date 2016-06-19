## Code:-> Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly interms of computation resources & time 
##Its beneficial to cache the inverse of a matrix rather than compute it 
##repeatedly.
## Following code demonstrate the caching functionality during the inverse of 
#matrix operation.
## In case of first time inversion of Matrix, the code does the actual 
##inversion of matrix.
## In case of repeated inversion of same matrix, the inversion data being taken
##from cache 


##Assumption -> The matrix supplied is always invertible.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) {
                invMatrix <<- inverse
        }
        getInv <- function() {
                invMatrix
        }
        list(set=set, get=get, setInv=setInv, getInv=getInv)
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invMatrix <- x$getInv()
        if(!is.null(invMatrix)) {
                message("getting cached data.") ##Cache data returned
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data) ##First time inversion
        x$setInv(invMatrix)
        invMatrix
}


##Following is some of the output when running the code
##> x = rbind(c(1,3),c(2,4)) ##rbind combines R Objects by Rows or Columns
##> m=makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(m)                       ##First Time
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(m)                       ##From cache
##getting cached data.
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(m)                       ##From cache
##getting cached data.
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> x = rbind(c(2,1),c(2,4))            ##Matrix changed
##> m=makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    2    1
##[2,]    2    4
##> cacheSolve(m)
##[,1]       [,2]                       ##First Time
##[1,]  0.6666667 -0.1666667
##[2,] -0.3333333  0.3333333
##> cacheSolve(m) 
##getting cached data.                  ##From cache
##[,1]       [,2]
##[1,]  0.6666667 -0.1666667
##[2,] -0.3333333  0.3333333
##>cacheSolve(m)
##getting cached data.                  ##From cache
##[,1]       [,2]
##[1,]  0.6666667 -0.1666667
##[2,] -0.3333333  0.3333333
##> 

