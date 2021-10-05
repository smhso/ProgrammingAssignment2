## So, Stephanie Mae 
## MATH 144

SO <- function(a = matrix()) {
  
  karugtong <- NULL 
  tama <- function(b){
    a <<- b
    karugtong <<- NULL
  }
  L <- function() {a}
  TI <- function(inverse) {karugtong <<- inverse}
  FI <- function() { karugtong }
  list(tama=tama, L=L, TI=TI, FI=FI)
}

SOLVE_CACHE_SO <- function(a, ... ) {
  
  karugtong <-  a$FI()
  
  if (!is.null (karugtong)) {
    message("invalid.")
    message("cacheing data.")
    
    return(karugtong)
  }
  
  synonym <- a$L()
  karugtong <- solve(synonym, ...)
  
  a$SI(karugtong)
  karugtong
}
