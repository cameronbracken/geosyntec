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
output_fields <- read.csv('field_list.csv')
field_type <- as.character(output_fields$Type)
field_type[field_type == 'Text'] <- 'character'
field_type[field_type == 'Double'] <- 'numeric'
field_type[field_type == 'Long Integer'] <- 'integer'
   # check all the tables
#tab <- sqlTables(channel,schema='bw0169_newdev1')

sa_name_map <- c("Roofs", "On Lot Impervious", "On Lot Landscaping", "Right of Way Impervious", 
"Right of Way Pervious", "Upgradient Undeveloped", "Downgradient Undeveloped")

feas_path <- 'Feasibility_Output'
path <- 'Pollutant Loading_Output/'
detail_ext <- '_WQ_detailed.txt'
feas_ext <- '_Feasibility.txt'
detail_files <- list.files(path,pattern='detail')
prefix <- str_split(detail_files,detail_ext)[[1]][1]
feas_files <- paste(prefix, feas_ext, sep='')
na.strings <- c("9999999", "N/A", "--")

for(i in 1:length(detail_files)){

        # read the detailed water quality output
    print_flush('Reading wq output...')
    this_detail_file <- file.path(path,detail_files[i])
    wq_con <- file(this_detail_file, "r")
    #wq_output <- read.csv(this_detail_file, na.strings=na.strings)
    #names(wq_output) <- scan(this_detail_file,what=character(),nlines=1,sep=',',quiet=TRUE)

        #read the feasibility output
    print_flush('Reading feasability output...')
    this_feas_file <- file.path(feas_path,feas_files[i])
        # the feasability output is malformed, need to add another column name
    feas_header <- c(scan(this_feas_file,what=character(),nlines=1,sep=',',quiet=TRUE), "OP_Cost")
    feas_con <- file(this_feas_file, "r")
    #feas_output <- read.csv(this_feas_file, header=F,skip=1,col.names=feas_header, na.strings=na.strings)
    #names(feas_output) <- str_trim(feas_header)

        # there is only one coopid per file, get that and query the 
        # water balance results from the database
    #print_flush('Pulling water balance results f...')
    #this_file_coopid <- as.numeric(str_split(wq_output$"Standard Area Unit ID"[1], '-')[[1]][2])
    #this_station_wb <- sqlQuery(channel,paste('SELECT * FROM qaqc_abbrev WHERE coopid="',this_file_coopid,'"',sep=''))
    #this_station_wb <- subset(feas_output, `Climate Station ID` == this_file_coopid)
    
        # unique sauids, this will stay the same for all the detailed result chunks
    #sauids <- unique(wq_output$`Standard Area Unit ID`)

        # presize a data frame to collect all these results together
        # for half packets add a row even if there is not a second bmp
    #print_flush('Allocating outpt data frame...')
    #nr <- 2 * nrow(wq_output)/length(unique(wq_output$`Contaminant Name`))
    #nc <- length(output_fields$Name)
    #out <- as.data.frame(matrix(NA,nrow=nr,ncol=nc))
    #names(out) <- output_fields$Name
        # specify the class of the columns
    #for(col in 1:nc)
    #    out[[col]] <- eval(call(paste('as',field_type[col],sep='.'),out[[col]]))
        # add a column for bmp index
    #out <- cbind(out,
    #        `BMP Index`=integer(1),
    #        `BMP Tributary Area (ft2)` = numeric(1)
    #        )

        # get the names of the pollutants
    #pollutants <- unique(wq_output$`Contaminant Name`)
    #source_areas <- unique(wq_output$`Source Area Name`)

    row <- 0 
    pb <- txtProgressBar(1,nr,style=3)
    print_flush('Setting Data Values...')
    for(j in 1:length(sauids)){

        feas_subset <- try(read.table(feas_con,nrows=1,sep=',',colClasses='character'), silent=TRUE)
        if(class(e) == 'try-error') break

        this_sauid <- sauids[i]
        #lookup <- sqlQuery(channel,paste("SELECT * FROM sauid_lookup WHERE sauid='",this_sauid,"'",sep=''))
        feas_subset <- subset(feas_output,`Standard Area Unit ID`==this_sauid & `Order Index` == 1)
        if(nrow(feas_subset) == 0) stop("No point feas_subseting wq output in the feasability output (this should not happen).")

        for(isa in 1:length(source_areas)){

            wq_subset <- subset(wq_output,`Standard Area Unit ID`==this_sauid & `Source Area Name` == source_areas[isa])
            this_saname <- as.character(wq_subset$`Source Area Name`[1])
            devsize <- str_split(feas_files,'_')[[1]][2] # get from file name

            for(ibmp in 1:2){
                row <- row + 1
                setTxtProgressBar(pb,row)

#browser()
if(FALSE){
                out[['Development Size']][row] <- devsize
                out[['Development Type']][row] <- strsplit(as.character(this_sauid),'-')[[1]][1] # get from sauid 
                out[['Major Land Use Classification']][row] <- gce(feas_subset, "Major Land Use Classification") 
                out[['Standard Land Development Model']][row] <- gce(feas_subset, "Standard Land Development Model") 
                out[['SLDM Impervious Fraction']][row] <- gce(feas_subset,"SLDM Impervious Fraction")
                out[['Climate Station ID']][row] <- gce(feas_subset,"SLDM Impervious Fraction")
                out[['Climate Station Name']][row] <- gce(feas_subset, "Climate Station City")
                out[['Climate Station State']][row] <- gce(feas_subset, "Climate Station State")
                out[['Soil Type']][row] <- gce(feas_subset, "Soil Type")
                out[['Infiltration Condition']][row] <- gce(feas_subset, "Infiltration Condition")
                out[['Existing Vegetation Value']][row] <- gce(feas_subset, "Existing Vegetation Value")
                out[['Standard']][row] <- gce(feas_subset, "Standard")
                out[['SA Number']][row] <- which(sa_name_map == this_saname)
                out[['Source Area Name']][row] <- this_saname
                
                #halfpacket <- ifelse()

                #if(ibmp == 2 & is.na(){
                # BMP volume is more complicated, pull the corresponding results from the 
                # feasiability output
                if(isa <= 5){

                    out[['BMP Family']][row] <- gce(feas_subset,"BMP ":isa:"-":ibmp:" Family")
                    out[['BMP Volume']][row] <- gce(feas_subset,"BMP ":isa:"-":ibmp:" Volume (ft3)")
                    out[['BMP Tributary Area']][row] <- gce(feas_subset,"BMP ":isa:"-":ibmp:" Tributary Area")

                    p_retain <- gne(feas_subset, "BMP ":isa:"-":ibmp:" % Rainfall Retained")
                    p_treat <- gne(feas_subset, "BMP ":isa:"-":ibmp:" % Rainfall Treated & Discharged")
                    p_bypass <- gne(feas_subset, "BMP ":isa:"-":ibmp:" % Rainfall Bypassed")
                    runoff_vol <- gne(wq_output, "Average Annual Runoff Volume w/o Controls (liters/SLDM Acre)")
                    
                }else{
                        # we are in an undeveloped area
                    out[['BMP Family']][row] <- NA
                    out[['BMP Volume (ft3)']][row] <- 0
                    out[['BMP Tributary Area']][row] <- NA

                    p_retain <- p_treat <- 0
                    p_bypass <- 1
                    runoff_vol <- NA
                }

                out[['Annual Runoff Volume']][row] <- runoff_vol
                out[['Annual Runoff Volume Retained']][row] <- runoff_vol * p_retain
                out[['Annual Runoff Volume Treated and Discharged']][row] <- runoff_vol * p_treat
                out[['Annual Runoff Volume Bypassed Without Treatment']][row] <- runoff_vol * p_bypass

                out[['BMP Index']][row] <- ibmp
                out[['Treated Surface Discharge Rate']][row] <- gne(feas_subset, "Minimally Impacting Treated Discharge (cfs/ac)")

                    #########################
                    # specific pollutants
                for(p in pollutants){
                    #print('a')
                    this_loading_noc <- subset(wq_subset, `Contaminant Name` == as.character(p))$"Annual Loading (w/o Controls)"
                    this_loading_c <- subset(wq_subset, `Contaminant Name` == as.character(p))$"Annual Loading (w/Controls)"

                    out[[p:' Retained']][row] <- this_loading_noc - this_loading_c # 9 
                    out[[p:' Bypassed']][row] <- this_loading_noc * p_bypass # 10
                    out[[p:' Treated and Discharged' ]][row] <- this_loading_c - (this_loading_noc * p_bypass) # 6 - 10
                
                }
                #browser()
}
            }
        }
    }
    close(pb)
}