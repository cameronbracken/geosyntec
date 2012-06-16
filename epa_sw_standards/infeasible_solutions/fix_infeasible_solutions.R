
library('RODBC')
library('xlsx')

source('lib_infeas_fix.R')

###################
#
# Start Working code
#
###################
#channel <- odbcConnect('epa_sw')

e2 <- try(typeof(channel),silent=TRUE)
    # Connect to the db and pull data
if(class(e2)=='try-error'){
    print_flush('Connecting to output database...')
    ptm <- proc.time()
    channel <- odbcConnect('epa_sw')
}

#bw0169_newdev2
#bw0169_redev1

#bw0169_n_ow_msa
#bw0169_n_ow_state
#bw0169_n_op_msa
#bw0169_n_op_state

#bw0169_r_ow_msa
#bw0169_r_ow_state
#bw0169_r_op_msa
#bw0169_r_op_state

dbs <- 'bw0169_newdev2'

#run_standards <- read.xlsx('run_standards.xlsx',sheetIndex=3)
#depths <- read.xlsx('Percentile_Storms_with_standards.xlsx',sheetName='depths_no_calcs')
print_flush('Reading Baseline Depths...')
depths <- read.xlsx('inferred_baseline_depths.xlsx',sheetName='Summary')
names(depths) <- tolower(names(depths))
depths$depth_state[depths$depth_state == 0] <- NA

cache_dir <- 'cache'
if(!file.exists(cache_dir)) dir.create(cache_dir)

for(dbi in 1:length(dbs)){
	print_flush('Using',dbs[dbi])
	sqlQuery(channel,paste("USE",dbs[dbi]))
	this_infeas_cache <- file.path(cache_dir, paste(dbs[dbi],'.RData',sep=''))

		# add column for flag to indicate which standard the 
		# infeasible solution was switched to
	print_flusht('Adding flag column to sauid_lookup table...')
	sqlQuery(channel,'alter table sauid_lookup add column infeas_sub_flag int')
	
		# get all the infeasible solutions from the database
	print_flusht('Pulling Infeasible Results...')
	if(!file.exists(this_infeas_cache)){
		infeas <- sqlQuery(channel,'select * from sauid_lookup where result_code=-1')
		nostd <- sqlQuery(channel,'select * from sauid_lookup where result_code=0')
		save(infeas,nostd,file=this_infeas_cache)
	}else{
		load(this_infeas_cache)
	}
	infeas_result_code <- infeas$result_code
	nostd_result_code <- nostd$result_code
	infeas_sauid <- as.character(infeas$sauid)
	nostd_sauid <- as.character(nostd$sauid)


		# get the list of standards from the database
	print_flusht('Pulling Standards Table...')
	standards <- sqlQuery(channel,'select * from standards')

	standards_db <- add_standards_column(standards)

	print_flusht('Fixing Infeasible Results...')
	pb <- num_progress_bar(1,nrow(infeas))
	for(rowi in 71107:nrow(infeas)){#nrow(infeas)){
		#setTxtProgressBar(pb,rowi)
		set_num_progress_bar(pb,rowi)

		this_infeas <- infeas[rowi,]
		standardid <- this_infeas$standardid

		this_depth <- depths[depths$coopid == this_infeas$coopid,]
		storm_depth <- make_percentile_df(this_depth)
		ms4 <- this_depth$depth_ms4

		standards <- add_depth_column(standards_db,this_depth)
		this_special <- standards$special[standardid]

		if(standardid <= 2){
			# baseline standard infeasible
			rep_std <- get_baseline_replacement(standards,standardid)

		}else{
			# rule standard infeasible
			retain_ms4 <- as.logical(depths$retain_ms4[depths$coopid == this_infeas$coopid])
			retain_state <- as.logical(depths$retain_state[depths$coopid == this_infeas$coopid])

			rep_std <- get_rule_replacement(standards,standardid,retain_ms4,retain_state)

		}
		rep_std <- get_actual_replacement(standards,standardid,this_special,rep_std,infeas_result_code,nostd_result_code,infeas_sauid,nostd_sauid)
		if(nrow(rep_std) != 1) browser()

		sqlQuery(channel, 
			paste('update sauid_lookup set infeas_sub_flag=',rep_std$standardid,
				" where sauid='",as.character(this_infeas$sauid),"'",sep=''))

		#infeas_result_code <- infeas_result_code[-1]
		#nostd_result_code <- nostd_result_code[-1]
		#infeas_sauid <- infeas_sauid[-1]
		#nostd_sauid <- nostd_sauid[-1]
		# match the infeasible standard to a depth in depths list

		# limit the matching by only those allowable depths for this run


  	}
  	close_num_progress_bar(pb)
}