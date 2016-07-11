
module_rank=function(X){
#input:
# X is a matrix of discriminant values measuring the correlations between modules (in rows) and variables/features (in columns)
#output:
# a vector of module ranking scores
	X=as.matrix(X)
	G=apply(X,2,function(y) {z=rank(y);M=max(z);(M+1-z)/M} )
	apply(G,1,function(x) sum(log(x)))
}

flatten.table <- function(master.table,factor.col,out.cols)
{
 # assume that first column marks test names
 if (length(factor.col) == 1)
 {
  factor.vec <- factor(master.table[[factor.col]])
 }else{
  factor.vec <- factor(apply(do.call(cbind,lapply(master.table[factor.col],as.character)),1,function(x) paste(x,collapse = "~")))
 }
 split.table <- lapply(split(1:nrow(master.table),factor.vec),function(ii,m) m[ii,],m = master.table)

 common.id <- Reduce("union",lapply(split.table,function(x) as.character(x[[1]])))
 
 big.table <- list()
 for (out.col in out.cols)
 {
  aligned.table <- do.call(cbind,lapply(split.table,function(tbl,id,out.col) {
                                         vec <- rep(NA,length(id));names(vec) <- id;

										 if (is.numeric(tbl[[out.col]])) vec[as.character(tbl[[1]])] <- as.numeric(tbl[[out.col]])
										 if (is.factor(tbl[[out.col]])) vec[as.character(tbl[[1]])] <- as.character(tbl[[out.col]])
										 return(vec)
                                 },id = common.id,out.col = out.col))
  colnames(aligned.table) <- paste(names(master.table)[out.col],colnames(aligned.table),sep = "__")
  big.table <- c(big.table,list(aligned.table))
 }
 big.table <- data.frame(module = common.id,do.call(cbind.data.frame,big.table))
 return(big.table)
}

combine.table <- function(abl,bbl)
{
 common.id <- union(as.character(abl[[1]]),as.character(bbl[[1]]))
 abl.align <- do.call(cbind,lapply(abl[2:ncol(abl)],function(x,y,z) {
                                   names(x) <- z;
                                   vec <- rep(NA,length(y));names(vec) <- y;
								   if (is.numeric(x)) {vec[names(x)] <- x;}else{
								   vec[names(x)] <- as.character(x);}
								   
								   return(vec)
                          },y = common.id,z = as.character(abl[[1]])))
 bbl.align <- do.call(cbind,lapply(bbl[2:ncol(bbl)],function(x,y,z) {
                                   names(x) <- z;
                                   vec <- rep(NA,length(y));names(vec) <- y;
								   if (is.numeric(x)) {vec[names(x)] <- x;}else{
								   vec[names(x)] <- as.character(x);}
								   
								   return(vec)
                          },y = common.id,z = as.character(bbl[[1]])))
 out <- cbind.data.frame(data.frame(id = common.id),abl.align,bbl.align)
 return(out) 
}


coerce.manyTables <- function(table.lst)
{
 if (length(table.lst) > 1)
 {
  out.table <- table.lst[[1]]
  for (i in 2:length(table.lst))
  {
   out.table <- combine.table(out.table,table.lst[[i]])
  }
 }else{
  out.table <- table.lst[[1]];
 }
 
 return(out.table)
}

