
library('RODBC')
channel <- odbcConnect('epa_sw')

dbs <- paste('bw0169',
	runs <- c('newdev2', 'redev1', 
	'n_ow_msa',  'r_ow_msa',
	'n_ow_state','r_ow_state',
	'n_op_msa',  'r_op_msa',
	'n_op_state','r_op_state'),sep='_')

#run_standards <- read.xlsx('run_standards.xlsx',sheetIndex=3)
depths <- read.xlsx('Percentile_Storms_with_standards.xlsx',sheetName='depths_no_calcs')
names(depths) <- tolower(names(depths))

for(dbi in 1){
	sqlQuery(channel,paste("USE",dbs[dbi]))

		# add column for flag to indicate which standard the 
		# infeasible solution was switched to
	sqlQuery(channel,'alter table sauid_lookup add column infeas_sub_flag int')
		# get all the infeasible solutions from the database
	infeas <- sqlQuery(channel,'select * from sauid_lookup where result_code=-1')
		# get the list of standards from the database
	standards <- sqlQuery(channel,'select * from standards')

	standards <- add_standards_column(standards)

	for(rowi in 1:nrow(infeas)){

		this_infeas <- infeas[rowi,]
		standardid <- this_infeas$standardid

		this_depth <- depths[depths$station_id == this_infeas$coopid,]
		storm_depth <- make_percentile_df(this_depth)
		replacements <- get_available_replacement_percentiles(storm_depth,ms4,standardid,standards)
		this_proposed <- standards$proposed[standardid]
		subset(standards, 
			standards$proposed == this_proposed 
			& standards$percentile %in% replacements$percentile 
			& special == 0)

		if(this_depth$standardid <= 2){
			# baseline standard infeasible

		}else{
			# rule standard infeasible

		}
		# get_percentiles_from_descriptions()
		# run_depths <- unique(sapply(strsplit(as.character(standards$standard),' - '),'[',2))
		# run_depths <- unique(substr(run_depths[!is.na(run_depths)],1,4))

		# get_potential_replacements 

		# std_desc <- subset(standards, standardid == this_infeas$standardid)$standard
		# this_percentile <- strsplit(as.character(std_desc),' - ')[[1]][2]
		# this_percentile <- substr(this_percentile, 1, 2)



		# match the infeasible standard to a depth in depths list

		# limit the matching by only those allowable depths for this run


  	}
}


make_percentile_df <- function(this_depth){
	data.frame(
		depth=sapply(this_depth[,6:10],function(x)x), 
		percentile=seq(75,95,by=5),
		name=paste(seq(75,95,by=5),'th',sep='')
	)

}

add_standards_column <- function(standards){

	names <- sapply(strsplit(as.character(standards$standard),' - '),'[',2)
	percentile <- as.numeric(substr(names, 1, 2))
	proposed <- as.numeric(sapply(strsplit(as.character(standards$standard),' '),'[',2))
	special <- ifelse(nchar(as.character(names))> 4, 1, 0)
	cbind(standards, names=names, percentile=percentile, proposed=proposed, special = special)

}

get_available_replacement_percentiles <- function(storm_depth,ms4,standardid,standards){

	this_proposed <- standards$proposed[standardid]
	p_match <- subset(standards, proposed == this_proposed)$percentile
	match_storm_depth <- subset(storm_depth, storm_depth$percentile %in% p_match)
	which_rank <- rank(c(ms4,match_storm_depth$depth))[1]
	replacements <- subset(match_storm_depth, match_storm_depth$depth < match_storm_depth$depth[which_rank])

	return(replacements)

}
