do.MEGENA <- function(g,
mod.pval = 0.05,hub.pval = 0.05,remove.unsig = TRUE,
min.size = 10,max.size = 2500,
doPar = FALSE,num.cores = 4,n.perm = 100,
save.output = FALSE)
{
	# g = igraph object of PFN
	# mod.pval = module p-value
	# hub.pval = hub p-value
	
	if (doPar & getDoParWorkers() == 1 & num.cores > 1)
	{
		cl <- makeCluster(n.cores)
		registerDoParallel(cl)
		# check how many workers are there
		cat(paste("number of cores to use:",getDoParWorkers(),"\n",sep = ""))
	}
	
	###### do clustering
	cat("Commence multiscale clustering....\n") 
	module.output <- nested.kmeans.all(g = g);
	save(module.output,file = "multiscale_clusters.RData")
	sig.modules <- module.output$modules[which(module.output$module.pvalue <= mod.pval )];
	sig.modules <- sig.modules[which(sapply(sig.modules,length) >= min.size & sapply(sig.modules,length) <= max.size)]
	
	if (save.output) output.geneSet.file(sig.modules,"multiscale_significant.modules.txt")
	
	###### perform hub analysis and scale clustering 
	cat("Commence MHA...\n")
	hub.output <- get.multiScale.hubs(module.output,g,
	module.degreeStat = NULL,doPar = doPar,n.core = num.cores,remove.unsig = remove.unsig,
	alpha.range = NULL,n.perm = n.perm,pval = mod.pval,padjust.method = "bonferroni")
    if (save.output) save(hub.output,file = "multiscale_hubAnalysis.RData")
	 
	node.table <- node.summary(PFN = g,module.output = module.output,hub.output = hub.output,module.pval = mod.pval,hub.pval = hub.pval)
	if (save.output) write.table(node.table,file = "multiscale_nodeSummary.txt",sep = "\t",row.names = F,col.names = T,quote = F)
	
	output <- list(module.output = module.output,hub.output = hub.output,node.summary = node.table);
	return(output)
}