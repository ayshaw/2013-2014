fdg1_coeffs=function(d=6,n.extent){
  ##d is dilation
  ##returns coeffecients of continuous Gauss 1st deriv transform, two-sided, n.extent = max # points on one side
  x=( (-n.extent):n.extent)
  y = -x*exp(-1*x*x/(2*d))/d
  NN=sum(exp(-1*x*x/(2*d)) )
  return(y/NN)
}
## d = "dilation" of the wavelet filter ?????? inverse of the scale parameter in the wavelet transform
