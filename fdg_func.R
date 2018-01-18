#fdg_func is the 1st derivitive guassian wavelet function of a 
#   given abscissa x and dilation d.
fdg_func <- function(x,d){
  y <- x*exp(-1*x*x/(2*d))/sqrt(d)
  return(y)
}
