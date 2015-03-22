## This script contains two functions : makeCacheMatrix and cache Solve 
## Together they can be used to avoid the repeated computation of the inverse of a matrix
## makeCacheMatrix creates a list of values of functions that are created inside of makeCacheMatrix
## cacheSolve either retrieves the already computed inverse or computes the inverse and saves it in 
## cache so that it can be retrieved and does not need to be retrieved. 

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL #creates null object inv_x
  set <- function(y) { # initializes function "set"
    x<<- y # initializes an object x in the environment of function makeCacheMatrix which is set to 
    # the argument of the function set (here called y) which seems to be [clarify] the argument to
    # the function makeCacheMatrix
    # sort of a y for the set-environemt
    inv_x <<- NULL # sets inv_x to null object in the environment of the makeCacheMatrix as well
  }
  get <- function() x # initalizes another function called get which retrieves the vector x from the 
  # makeCacheMatrix environment 
  set_inv <- function(inv) inv_x <<- inv # creates another function  "set_inv" which takes as input a 
  # a value inv_x in the environment of makeCacheMatrix to this value
  get_inv <- function() inv_x # creates another function called "get_inv" which gets the value inv_x
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv) # makes a list which 
  # lists the values of the four functions created inside of function "makeCacheMatrix"
}

# cacheSolve : retrieves the already computed inverse or computes the inverse and saves it in cache
cacheSolve <- function(makeCacheMatrix, ...) {
       inv_x <- x$get_inv() # retrieves inv_x from the environment of makeCacheMatrix
       if (!is.null(inv_x)){ ## if inv_x is not zero (has been calculated)
         message("getting cached data") # prints this message on the screen
         return(inv_x) # returns inv_x from, already calculated
       }
       data <- x$get() # initializes variable data which is the value of the "get" function 
       # defined in the makeCacheMatrix environment
       inv_x <- solve(data, ...) # calculates the inverse of data 
       x$setmean(inv_x) # sets the mean in the makeCacheMatrix environement to inv_x so that inv_x is
       # now stored in cache and can be retrieved 
}
