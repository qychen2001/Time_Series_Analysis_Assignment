my_array=c(1,2,3,4,5,6,7,8)

min_max=function(input_array){
  return_min=Inf
  return_max=-Inf
  for (i in input_array){
    if (i>return_max) return_max=i
    if (i<return_min) return_min=i
  }
  c(return_max,return_min)
}
min_max(my_array)