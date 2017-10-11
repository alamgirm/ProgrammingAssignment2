## This assignment demonstrates how R can cache objects to save
##   computations on subsequent calls.
##   A matrix can be created with calling makeMatrix()
##  An inverse of this matrix can be computed by calling
##  cacheSolve(m).
##  cacheSolve will call R solve on the first instance.
##  subsequent calls to caseSolve will supply the inverse from cache

## Example:
## Create a non-singular matrix:
## > m = makeMatrix(matrix(c(1,2,3, 2,1, 3, 3,2,1),3,3))
## now call cacheSolve(m) twice
## > cacheSolve(m)
## > cacheSolve(m)
## on the second call, it should show a message that the inverse
## is being served from cache
## Use this special function to create matrix

makeMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function (y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() { x }
    setMatrixInverse <- function (inv) { inv_m <<- inv }
    getMatrixInverse <- function() { inv_m }
    
    list( set = set, get = get, 
          setMatrixInverse = setMatrixInverse,
          getMatrixInverse = getMatrixInverse)
}


## This function can cache matrix inverse, and
## save call to solve() on subseqent inverse request

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m_inv <- x$getMatrixInverse()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return (m_inv)
    }
    data  <- x$get()
    m_inv <- solve(data,...)
    x$setMatrixInv(m_inv)
    
    m_inv
}
