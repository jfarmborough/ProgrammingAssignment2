source('D:/Coursera/2_RProgramming/Week3/ProgrammingAssignment2/cachematrix.R', echo=FALSE)
mat <- matrix(c(4,3,6,3), nrow=2,ncol=2) 
cacheMatrix <- makeCacheMatrix(mat)
cacheSolve(cacheMatrix)
cacheSolve(cacheMatrix)