## A function to create an inverse of inversible matrices
## The function will check if the inverse is already created
## If the inverse is already created, it will fetch the inversed matrix from cache memory
## If it is not calculated already, the function will inverse the matrix and assign it to cache for later purpose
## next time, it will retrieve from the cached memory
## This function is useful when the inverse has to be used multiple times 
## As it will be stored in the cache, can be retrived everytime and need not derive everytime which saves times and memory

## .....................................makeCacheMatrix Function .......................
## define Caching function which will return a list of four functions 
## the functions in the list will be used to call defining or returning the martrix or it's inversed value
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL   # Assigning the Inverse Matrix variable
          ## ......... set function ......
  set <- function(y) {  # Defining the function to create the matrix
    x <<- y             # Assigning the matrix value
    i <<- NULL
  }
          ## ..... get function .....
  get <- function() x   # Function to return the matrix
          ## .......setinv function.....
  setinv <- function(solve) i <<- solve   # Function to assing the inversed matrix variable(Caching for later purpose)
          ##........getinv function.......
  getinv <- function() i  # Returns the inversed matrix
  
  # Returns a list of set, get, setinv, getinv functions
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

##.....................cacheSolve......................
## function to check if the inverse is already determined and determine if needed

cacheSolve <- function(x, ...) {    # Defining the function
  i <- x$getinv()   # Assigning the inversed value using getinv function of the list "x"
  if(!is.null(i)) { # Checks if the matrix is already inversed
    message("getting cached data")  # Prints if the inverse is already calculated just as a confirmation it is using cache
    return(i)   # Retuns the inversed matrix and exits the function
  }
  
  # If the inverse is not already derived, it will proceed with deriving with the following function
  
  data <- x$get()   # Assigns the matrix for which the inverse has to be derived
  i <- solve(data, ...) # Using solve function to derive the inverse
  x$setinv(i)   # Assigning the derived inverse matrix to cache memory for later retival
  i # returns the inversed matrix
}
