library("RODBC")
library("plyr")
library("stringr")
source('lib.R')

e2 <- try(typeof(channel_out),silent=TRUE)
    # Connect to the db and pull data
if(class(e2)=='try-error'){
    print_flush('Connecting to output database...')
    ptm <- proc.time()
    channel_out <- odbcConnect('epa_sw_local')
    sqlQuery(channel_out,"USE wq_detailed_test")
}

coopids <- scan('detailed_wq_stations.txt',quiet=TRUE)
#output_fields <- read.csv('field_list2.csv')
#field_type <- as.character(output_fields$Type)
#field_type[field_type == 'Text'] <- 'character'
#field_type[field_type == 'Double'] <- 'numeric'
#field_type[field_type == 'Long Integer'] <- 'integer'
   # check all the tables
#tab <- sqlTables(channel,schema='bw0169_newdev1')

sa_name_map <- c("Roofs", "On Lot Impervious", "On Lot Landscaping", "Right of Way Impervious", 
"Right of Way Pervious", "Upgradient Undeveloped", "Downgradient Undeveloped")

feas_path <- 'Feasibility_Output'
path <- 'Pollutant Loading_Output/'
path_out <- 'Pollutant Loading_Output_Combined/'
bmp_loading_ext <- '_WQ_bmp_loading.txt'
detail_ext <- '_WQ_detailed.txt'
feas_ext <- '_Feasibility.txt'
detail_files <- list.files(path,pattern='bmp_loading')
prefix <- sapply(str_split(detail_files,bmp_loading_ext),'[',1)
feas_files <- paste(prefix, feas_ext, sep='')
na.strings <- c("9999999", "N/A", "--")

for(i in 1:1){#length(detail_files)){

    out_file <- file.path(path_out,paste(prefix[i],bmp_loading_ext,sep=''))
    out_con <- file(out_file, open='wa')

    print_flush(detail_files[i])
        # read the detailed water quality output
    this_detail_file <- file.path(path,detail_files[i])
    wq_con <- file(this_detail_file, "r")
    wq_header <- scan(wq_con,what=character(),nlines=1,sep=',',quiet=TRUE)


        #read the feasibility output
    this_feas_file <- file.path(feas_path,feas_files[i])
    feas_con <- file(this_feas_file, "r")
        # the feasability output is malformed, need to add another column name
    feas_header <- c(scan(feas_con,what=character(),nlines=1,sep=',',quiet=TRUE), "OP_Cost")

    devsize <- as.numeric(str_split(feas_files,'_')[[1]][2]) # get from file name

    detail_sample <- read.table(this_detail_file,nrows=7*15,sep=',',colClasses='character',
            comment.char='', na.strings = na.strings, header=TRUE)
    devtype <- strsplit(as.character(detail_sample[1,1]),'-')[[1]][1]

    source_areas <- unique(detail_sample$'Source.Area')
    pollutants <- unique(detail_sample$'Contaminant.Name')
    pollutant_output_names <- character(0)
    for(i in 1:length(pollutants)){
        pollutant_output_names <- c(pollutant_output_names, paste(pollutants[i], c('Retained','Treated and Discharged','Bypassed')))
    }
    header_out <- c("sauid", "Source Area Number", "Source Area Name", "BMP Index", 
        "BMP Family", "BMP Volume", "BMP Tributary Area", "Annual Runoff Volume", 
        "Annual Runoff Volume Retained", "Annual Runoff Volume Treated and Discharged", 
        "Annual Runoff Volume Bypassed Without Treatment", "Total SDRate")

    header_out <- c(header_out, pollutant_output_names)
    cat(header_out, file=out_con, sep=',')
    writeLines('', con=out_con)
    
    row <- 0 
    print_flush('Setting Data Values...')
    repeat{
            ##############################
            # read the feasability file
            ###############################
        feas_subset <- try(read.table(feas_con,nrows=1,sep=',',colClasses='character',comment.char='', na.strings = na.strings), silent=TRUE)
            # if we are at the end of the file an error occurs, so break
        if(class(feas_subset) == 'try-error') break
            # the next 11 lines are the 2-12 best options, we only need the first
        readLines(feas_con,n=11)
        names(feas_subset) <- feas_header

        this_sauid <- feas_subset[['Standard Area Unit ID']]


        for(isa in 1:length(source_areas)){

            sa_subset <- try(read.table(wq_con,nrows=15,sep=',',colClasses='character',comment.char='', na.strings = na.strings), silent=TRUE)
            if(class(sa_subset) == 'try-error') stop('Reached the end of the wq file before the end of the feas file, this is bad.')
            
            names(sa_subset) <- wq_header
            this_saname <- as.character(source_areas[isa])

            for(ibmp in 1:2){
                row <- row + 1

                write_csv_e(this_sauid, out_con)
                #write_csv_e(devsize, out_con)
                #write_csv_e(devtype, out_con)
                #write_csv_e(gce(feas_subset, "Major Land Use Classification"),out_con)
                #write_csv_e(gce(feas_subset, "Standard Land Development Model"),out_con)
                #write_csv_e(gce(feas_subset, "SLDM Impervious Fraction"),out_con)
                #write_csv_e(gce(feas_subset, "Climate Station ID"),out_con)
                #write_csv_e(gce(feas_subset, "Climate Station City"),out_con)
                #write_csv_e(gce(feas_subset, "Climate Station State"),out_con)
                #write_csv_e(gce(feas_subset, "Soil Type"),out_con)
                #write_csv_e(gce(feas_subset, "Infiltration Condition"),out_con)
                #write_csv_e(gce(feas_subset, "Existing Vegetation Value"),out_con)
                #write_csv_e(gce(feas_subset, "Standard"),out_con)
                write_csv_e(isa,out_con) # Source Area Number
                write_csv_e(this_saname,out_con)
                write_csv_e(ibmp, out_con)

                # BMP volume is more complicated, pull the corresponding results from the 
                # feasiability output
                bmp_trib_area <- gne(feas_subset,"BMP ":isa:"-":ibmp:" Tributary Area")
                #annual_rain <- gne(sa_subset, "Average Annual Precipitation (inches)")
                roc <- gne(sa_subset, "Volumetric Runoff Coefficient")
                #ann_ro_vol <- annual_rain*roc*bmp_trib_area/devsize*43560/12
                    # convert liters to ft^3
                ann_ro_vol <- gne(sa_subset, "Average Annual Runoff Volume w/o Controls (liters/SLDM Acre)")*28.3168466
               
                if(isa <= 5){

                    halfpacket <- ifelse(is.na(gce(feas_subset,"BMP ":isa:"-":2:" Family")), FALSE, TRUE)
                    write_csv_e(gce(feas_subset,"BMP ":isa:"-":ibmp:" Family"), out_con)
                    write_csv_e(gce(feas_subset,"BMP ":isa:"-":ibmp:" Volume (ft3)"), out_con)
                    write_csv_e(bmp_trib_area, out_con)

                    if((ibmp == 2 & isTRUE(halfpacket)) | ibmp == 1){

                        p_ro_ret <- suppressWarnings(gne(feas_subset, "BMP ":isa:"-":ibmp:" % Rainfall Retained"))/roc
                        p_ro_td <- suppressWarnings(gne(feas_subset, "BMP ":isa:"-":ibmp:" % Rainfall Treated & Discharged"))/roc
                        p_ro_byp <- suppressWarnings(gne(feas_subset, "BMP ":isa:"-":ibmp:" % Rainfall Bypassed"))/roc

                    }else{
                        p_ro_ret <- p_ro_td <- p_ro_byp <- ann_ro_vol <- NA
                    }
                    
                }else{
                        # we are in an undeveloped area, there is no bmp
                    halfpacket <- FALSE
                    write_csv_e("", out_con) # BMP Family
                    write_csv_e(0, out_con) # Volume
                    write_csv_e(bmp_trib_area, out_con) # BMP Trib Area

                    if((ibmp == 2 & isTRUE(halfpacket)) | ibmp == 1){
                        p_ro_ret <- p_ro_td <- 0
                        p_ro_byp <- 1
                    }else{
                        p_ro_ret <- p_ro_td <- p_ro_byp <- ann_ro_vol <- NA
                    }
                }
                #if(isa == 6) browser()

                ann_ro_td = ann_ro_vol * p_ro_td
                ann_ro_byp = ann_ro_vol * p_ro_byp
                ann_ro_ret = ann_ro_vol - (ann_ro_byp + ann_ro_td)

                write_csv_e( ann_ro_vol, out_con ) #Annual Runoff Volume
                write_csv_e( ann_ro_ret, out_con )#Annual Runoff Volume Retained
                write_csv_e( ann_ro_td , out_con ) #Annual Runoff Volume Treated and Discharged
                write_csv_e( ann_ro_byp, out_con ) #Annual Runoff Volume Bypassed Without Treatment
                #{[Annual Volume Treated and Discharged]/([Annual Volume Treated and Discharged] + [Annual Volume Retained])}*([BMP Volume]/[Drawdown Time
                write_csv_e( gne(feas_subset, "Minimally Impacting Treated Discharge (cfs/ac)"), out_con)

                    #########################
                    # specific pollutants
                for(p in 1:length(pollutants)){
                    #print('a')
                    if((ibmp == 2 & isTRUE(halfpacket)) | ibmp == 1){
                        this_loading_noc <- gne(sa_subset,"Annual Loading (w/o Controls)",p)
                        this_loading_c <- gne(sa_subset,"Annual Loading (w/Controls)",p)
                    }else{
                        this_loading_noc <- this_loading_c <- NA
                    }

                    load_ret <- this_loading_noc - this_loading_c # 9
                    load_td <- this_loading_c - (this_loading_noc * p_ro_byp) # 10
                    load_byp <- this_loading_noc * p_ro_byp # 6-10

                    write_csv_e(load_ret, out_con) # 9 
                    write_csv_e(load_td, out_con) # 10

                    if(p == length(pollutants)) 
                        write_csv_e(load_byp, out_con, end=TRUE) 
                    else
                        write_csv_e(load_byp, out_con) 
                } 
                #this_loading_noc - (this_loading_noc - this_loading_c + this_loading_noc * p_ro_byp / roc)
                browser()
            }
           # browser()
        }
    }
    #close(pb);
    close(feas_con);close(wq_con);close(out_con)

    #print_flush('Writing to the database...')
    #this_table <- read.csv(out_file)
    #names(this_table) <- scan(out_file,what=character(),nlines=1,sep=',',quiet=TRUE)
    #if(i == 1){
    #    sqlSave(channel_out, this_table, tablename='detailed_results', rownames=FALSE, fast=TRUE, safer=FALSE)
    #}else{
    #    sqlSave(channel_out, this_table, tablename='detailed_results', rownames=FALSE, fast=TRUE, append=TRUE)
    #}

}