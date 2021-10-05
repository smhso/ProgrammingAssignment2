## So, Stephanie Mae 
## MATH 144

SO <- function(a = matrix()) {
  
  karugtong <- NULL 
  tama <- function(b){
    a <<- b
    karugtong <<- NULL
  }
  H <- function() {a}
  LI <- function(inverse) {karugtong <<- inverse}
  PI <- function() { karugtong }
  list(tama=tama, H=H, LI=LI, PI=PI)
}

SOLVE_CACHE_SO <- function(a, ... ) {
  
  karugtong <-  a$LI()
  
  if (!is.null (karugtong)) {
    message("invalid.")
    message("cacheing data.")
    
    return(karugtong)
  }
  
  synonym <- a$H()
  karugtong <- solve(synonym, ...)
  
  a$PI(karugtong)
  karugtong
}

