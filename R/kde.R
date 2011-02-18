kde <-
function(type_kernel="n",vec_data,
y=seq(min(vec_data), max(vec_data), length.out=100),
bw=PBbw(type_kernel="n", vec_data, 2))
# INPUTS:
#   "type_kernel" kernel function: "e" Epanechnikov,	"n" Normal, 
#                                  "b" Biweight, "t" Triweight         
#   "vec_data" sample of data 
#   "y" grid where the kernel estimation is computed
#   "bw" bandwidth
# OUTPUT:Returns a list containing:
#    "Estimated_values" vector containing the estimated function
#    in the grid values
#    "grid" the used grid 
#    "bw" value of the current bandwidth
{
		n <- length(vec_data)
		aux <- outer(y,vec_data,"-")
	 	aux <- kernel_function_distribution(type_kernel, aux/bw)
		result <- apply(aux,1,mean)
  	return(list(Estimated_values = result, grid=y, bw = bw))
}

