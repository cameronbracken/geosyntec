
library('RODBC')
channel <- odbcConnect('epa_sw')

dbs <- paste('bw0169',
	runs <- c('newdev2', 'redev1', 
	'n_ow_msa',  'r_ow_msa',
	'n_ow_state','r_ow_state',
	'n_op_msa',  'r_op_msa',
	'n_op_state','r_op_state'),sep='_')

for(i in 1){#length(dbs)){

	sqlQuery(channel,paste("USE",dbs[i]))

	ptm <- proc.time()
	assign(runs[i],sqlFetch(channel,"qaqc"))
	save(list=runs[i],file=paste(runs[i],'.RData',sep=''))
	cat(paste("Pulled",runs[i],"data in",round((proc.time() - ptm)[3]/60,2),"minutes\n"))
	flush.console()

}
#obj = local(get(load('myfile.RData')))