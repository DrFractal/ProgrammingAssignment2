#############################
## Cached Matrix Inversion ##
## Author: DrFractal       ##
#############################

#############################################
## R Programming, Programming Assignment 2 ##
#############################################
##
## Finds the inverse of a matrix and caches it, so that future calls return the 
## cached inverse.
##
## There are two functions:
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache 
##   its inverse. 
##   In effect, makeCacheMatrix acts like a class and calling it results in an 
##   object with methods specified in the returned list.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix above.
##   If the inverse has already been calculated (and the matrix has not changed), 
##   then cachesolve retrieves the inverse from the cache.
##   Uses 'solve' to calculate the inverse of a square matrix. 
##   Assumes that the matrix supplied is square and invertible.
##################################################################################


##################################################################################
## makeCacheMatrix: This function creates a special "matrix" object that can cache 
##   its inverse.
##################################################################################
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  getx <- function() x
  setx <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getinverse <- function() inverse
  setinverse <- function(inv){
    inverse <<- inv
  }
  list(getx = getx, 
       setx = setx, 
       getinverse = getinverse, 
       setinverse = setinverse) ## return the list of sub-functions
}

###################################################################################
## cacheSolve: This function computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix above.
##   If the inverse has already been calculated (and the matrix has not changed), 
##   then cachesolve retrieves the inverse from the cache.
##   Assumes that the matrix supplied is square and invertible.
##   Internally uses 'solve' to calculate the inverse of a square matrix. 
##   If the matrix in x is set to NULL, it will return NULL.
##   Return a matrix that is the inverse of 'x', assumes x is a square 
##   invertible matrix.
###################################################################################
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if( !is.null(inverse) ){
    message("returning inverse from cache")
    return(inverse)
  }
  data <- x$getx()
  if(!is.null(data)){        ## check to be sure x data isn't null
    inverse <- solve(data)   ## find the inverse at last!
    x$setinverse(inverse)    ## set the inverse, so it's cached
  }
  inverse                    ## return the inverse
}

####################
##### End Code #####
####################

##################
##### TESTS ######
##################

############################
##### Passes All Tests #####
############################

###############
##### Test 1 ##
###############
# m1 <- matrix( 
# c(2, 4, 3, 1), # the data elements 
# nrow=2,        # number of rows 
# ncol=2,        # number of columns 
# byrow = TRUE)  # fill matrix by rows
# matt <- makeCacheMatrix(m1)
# cacheSolve(matt)
# cacheSolve(matt) ## do it again and show off the cache
#
######################
##### Test 1 Result ##
######################
##      [,1] [,2]
## [1,] -0.1  0.4
## [2,]  0.3 -0.2
## Cached version returns the same result, and prints
## "returning inverse from cache"

####################################
##### Test 2: 2x2 Identity Matrix ##
####################################
# m1 <- matrix( 
# c(1, 0, 0, 1), # the data elements 
# nrow=2,        # number of rows 
# ncol=2,        # number of columns 
# byrow = TRUE)  # fill matrix by rows
# matt <- makeCacheMatrix(m1)
# cacheSolve(matt)
# cacheSolve(matt) ## do it again and show off the cache
#
######################
##### Test 2 Result ##
######################
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
#
## Cached version returns the same result, and prints
## "returning inverse from cache"

####################################
##### Test 3: 3x3 Identity Matrix ##
####################################
# m1 <- matrix( 
# c(1, 0, 0, 
#   0, 1, 0,
#   0, 0, 1),    # the data elements 
# nrow=3,        # number of rows 
# ncol=3,        # number of columns 
# byrow = TRUE)  # fill matrix by rows
# matt <- makeCacheMatrix(m1)
# matt$getx()
# cacheSolve(matt)
# cacheSolve(matt) ## do it again and show off the cache
#
######################
##### Test 3 Result ##
######################
#      [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
# 
## Cached version returns the same result, and prints
## "returning inverse from cache"

#################################################################################
##### Test 4: 3x3 Matrix: Rotate about z by angle, shrinking or expanding by r ##
#################################################################################
# r <- (sqrt(5) -1 )/2
# angle <- (pi / 4)
# y1 <- (r * sin(angle))
# x1 <- (r * cos(angle))
# m1 <- matrix(
# 
# c(x1, -y1, 0, 
#   y1, x1, 0,
#   0, 0, 1),    # the data elements 
# nrow=3,        # number of rows 
# ncol=3,        # number of columns 
# byrow = TRUE)  # fill matrix by rows
# matt <- makeCacheMatrix(m1)
# matt$getx()
# cacheSolve(matt)
# cacheSolve(matt) ## do it again and show off the cache
#
######################
##### Test 4 Result ##
######################
#           [,1]     [,2] [,3]
# [1,]  1.144123 1.144123    0
# [2,] -1.144123 1.144123    0
# [3,]  0.000000 0.000000    1
# 
## Cached version returns the same result, and prints
## "returning inverse from cache"
