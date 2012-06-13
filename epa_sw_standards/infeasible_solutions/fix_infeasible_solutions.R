
library('RODBC')
library('xlsx')

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

get_rule_replacement <- function(standards,standardid,retain_ms4,retain_state){

	this_storm_depth <- standards$depth[standardid]
	this_proposed <- standards$proposed[standardid]
	this_special <- standards$special[standardid]
		# if we are on a regular standard (not a 1-inch cap), then we want to 
		# discard any 1-inch cap standards because we will have a regualr standard to 
		# use, but if we are on a 1-inch cap standard then we can just include the
		# regular standards without doing anything
	special_filter <- if(!this_special) standards$special == this_special else rep(TRUE,nrow(standards))
	rep <- subset(standards, 
		depth < this_storm_depth & 
		(proposed == this_proposed | is.na(proposed)) &
		special_filter)

	loc_ms4 <- rep$standard == 'Existing--MS4'
	loc_state <- rep$standard == 'Existing--State'
	if(!retain_ms4 & any(loc_ms4)){
		# this state does not have a retention standard so delete the 
		# standard from the list of possible replacements
		rep <- subset(rep,!loc_ms4)
	}
	if(!retain_state & any(loc_state)){
		# this state does not have a retention standard so delete the 
		# standard from the list of possible replacements
		rep <- subset(rep,!loc_state)
	}

	return(rep)

}

get_baseline_replacement <- function(standards,standardid){

	this_storm_depth <- standards$depth[standardid]
		# return all the retain standards with depths less than the current depth
	rep <- subset(standards, standards$depth < this_storm_depth & proposed == 4 )

	return(rep)

}

add_depth_column <- function(standards,this_depth){
	std_depths <- numeric(nrow(standards))
	std_depths[1] <- this_depth$depth_state
	std_depths[2] <- this_depth$depth_ms4
	storm_depth <- make_percentile_df(this_depth)

	for(i in 3:nrow(standards))
		std_depths[i] <- storm_depth$depth[storm_depth$percentile == standards$percentile[i]]
	cbind(standards,depth=std_depths)
}


print_flush <- function(...){

	cat(...,'\n')
	flush.console()
}

print_flusht <- function(...){

	cat('\t',...,'\n')
	flush.console()
}


make_std_sauids <- function(row,standards){
	exp_sauid <- strsplit(as.character(row[1,]$sauid),'-')[[1]]
	beg <- paste(exp_sauid[1:3],collapse='-')
	mid <- standards$standardid
	end <- paste(exp_sauid[5:7],collapse='-')
	paste(beg,mid,end,sep='-')
}

get_closest_lt_percentile <- function(standards, standardid){

	this_percentile <- standards$percentile[standardid]
	if(is.na(this_percentile)){
		sub <- subset(standards, proposed == 4)
		lt <- sub$depth < standards$depth[standardid]
		return(sub$percentile[lt][which.min(sub$depth[lt])])
	}else{
		return(this_percentile)
	}

}

get_actual_replacement <- function(standards,standardid,rep_std){
	if(nrow(rep_std) == 0){
		# there are no available replacements, sub the closest treat and release standard
		this_percentile <- get_closest_lt_percentile(standards,standardid)
		return(subset(standards, percentile == this_percentile & proposed == 3 & special == 0))
	}else{
		#check for feasiblity of other standards
		check_ord <- order(rep_std$depth,decreasing=TRUE)
		possible_sauids <- make_std_sauids(this_infeas,rep_std)
		not_found_replacement <- TRUE
		i <- 0
		while(not_found_replacement){
			i <- i + 1
			if(i == length(check_ord)){
				break
			}else{
				code <- sqlQuery(channel,paste("select result_code from sauid_lookup where sauid='",possible_sauids[check_ord[i]],"'",sep=''))
				if(as.numeric(code) == 1)
					not_found_replacement <- FALSE
			}
		}
		if(not_found_replacement){
			this_percentile <- get_closest_lt_percentile(standards,standardid)
			return(subset(standards, percentile == this_percentile & proposed == 3))
		}else{
			return(standards[i,])
		}
	}
}

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

dbs <- paste('bw0169',
	runs <- c('newdev2'#, 'redev1', 
	#'n_ow_msa',  'r_ow_msa',
	#'n_ow_state','r_ow_state',
	#'n_op_msa',  'r_op_msa',
	#'n_op_state','r_op_state'
	),sep='_')

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
		save(infeas,file=this_infeas_cache)
	}else{
		load(this_infeas_cache)
	}

		# get the list of standards from the database
	print_flusht('Pulling Standards Table...')
	standards <- sqlQuery(channel,'select * from standards')

	standards_db <- add_standards_column(standards)

	print_flusht('Fixing Infeasible Results...')
	pb <- txtProgressBar(1,nrow(infeas),style=3)
	for(rowi in 1:nrow(infeas)){
		setTxtProgressBar(pb,rowi)

		this_infeas <- infeas[rowi,]
		standardid <- this_infeas$standardid

		this_depth <- depths[depths$coopid == this_infeas$coopid,]
		storm_depth <- make_percentile_df(this_depth)
		ms4 <- this_depth$depth_ms4

		standards <- add_depth_column(standards_db,this_depth)


		if(standardid <= 2){
			# baseline standard infeasible
			rep_std <- get_baseline_replacement(standards,standardid)

		}else{
			# rule standard infeasible
			retain_ms4 <- as.logical(depths$retain_ms4[depths$coopid == this_infeas$coopid])
			retain_state <- as.logical(depths$retain_state[depths$coopid == this_infeas$coopid])

			rep_std <- get_rule_replacement(standards,standardid,retain_ms4,retain_state)

		}
		rep_std <- get_actual_replacement(standards,standardid,rep_std)
		if(nrow(rep_std) != 1) browser()

		# match the infeasible standard to a depth in depths list

		# limit the matching by only those allowable depths for this run


  	}
  	close(pb)
}
