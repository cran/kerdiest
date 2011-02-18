mrp <-
function(type_kernel="n",vec_data,
y=seq(min(vec_data), max(vec_data), length.out=50),
bw=PBbw(type_kernel="n", vec_data, 2), landa)
# INPUTS:
#   "type_kernel" kernel function: "e" Epanechnikov,	"n" Normal, 
#                                  "b" Biweight, "t" Triweight         
#   "vec_data" data sample
#   "y" grid where the kernel estimation is computed
#    "landa" mean activity rate
#   "bw" bandwidth
# OUTPUT:Returns a list containing:
#    "Estimated_values" vector containing estimated reponses for 
#                        each curve of "CURVES"
#     "y " the grid
#    "bw" value of the  bandwidth
{
		aux <- outer(y,vec_data,"-")
	 	aux <- kernel_function_distribution(type_kernel, aux/bw)
		result <- apply(aux,1,mean)
		mrp_result<-1/(landa*(1-result))
  	return(list(Estimated_values = mrp_result, grid=y, bw = bw))
}

