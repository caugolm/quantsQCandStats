#' Bark x times
#' 
#' @param x A numeric value to specify the number of times the dog barks
#' @return The barks
#' @export
bark <- function(x) {
  dog <- "woof"
  for (i in 1:x) {
    dog <- paste(dog, "woof")
  }
  return(dog)
}
