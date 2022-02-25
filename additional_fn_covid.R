


### function: use the first row of the data as the header.
firstrowhead <- function(data){
  names(data) <- as.matrix(data[1,])
  data<-data[-1,]
  data[]<-lapply(data, function(x) type.convert(as.character(x)))
  return(data)
}

