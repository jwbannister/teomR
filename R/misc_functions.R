
#' round a number to closest multiple of a given number.
#' 
#' @param x Numeric.
#' @param base Numeric.
#' @return x rounded to closest multiple of base.
#' @examples
#' > mround(11.3, 5)
#' > 10
mround <- function(x, base){
  base * round(x/base)
}
