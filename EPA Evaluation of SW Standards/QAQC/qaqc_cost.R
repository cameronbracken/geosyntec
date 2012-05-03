library("RODBC")
library("plyr")
library("stringr")
library("scales")
library("reshape2")
library("ggplot2")
library("xlsx")

plot_dir <- 'plots'
table_dir <- 'tables'
results_file <- 'qaqc_results.RData'
connect_and_pull <- FALSE
save_data <- FALSE
load_data_from_file <- TRUE
bar_only <- FALSE # only produce bar plots 
sample_3 <- FALSE # only sample three sites randomly for speedy prototyping

    # create the plot directory if it does not exist
if(!file.exists(plot_dir)) dir.create(plot_dir)
if(!file.exists(table_dir)) dir.create(table_dir)

    # Connect to the db and pull data
if(connect_and_pull){
    cat('Connecting to database...\n')
    ptm <- proc.time()
    flush.console()
    channel <- odbcConnect('epa_sw')
    sqlQuery(channel,"USE bw0169_newdev2")

	   # check all the tables
    #tab <- sqlTables(channel,schema='bw0169_newdev1')
	   # takes a minute or so
    cat('Pulling results table...\n')
    flush.console()
    res <- sqlFetch(channel,"qaqc_abbrev")
    names(res) <- c("sauid", "devtype", "coopid", "station_name", "state", "sldmname", 
        "standard", "vegval", "soiltype", "infcond", "devsize", "roc", "result_code", 
       "tpv", "total_storage", "p_retained", "p_treated", "p_bypass")
    cat(paste("Pulled data in",round((proc.time() - ptm)[3]/60,2),"minutes\n"))
}

    # optionally save the data to a binary R file for quick access later
if(save_data)
    save(res,file='qaqc_results.RData')

    # optionally load the data quickly from a local file if it exists
if(load_data_from_file){
    if(file.exists(results_file))
        load(results_file)
    else
        stop('No Results DB to Load.')
}

    # unique names
u_coopid <- unique(res$coopid)
u_soiltype <- unique(res$soiltype)
u_infcond <- unique(res$infcond)
    # capitalize the first letter of each word 
simpleCap <- function(x) {
    s <- strsplit(as.character(x), " ")[[1]]
    paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
          sep="", collapse=" ")
}

res$soil_inf <- as.factor(paste(res$soiltype,' (',res$infcond,')',sep=''))

pb <- txtProgressBar(1,length(u_coopid),style=3)
count <- 0
r <- c(10^3, 10^7)

site_range <- if(isTRUE(sample_3)) sample(1:length(u_coopid), 3) else 1:length(u_coopid)
    # main loop for each coop id
for(icoopid in site_range){#length(u_coopid)){

    this_station_data <- subset(res, coopid==u_coopid[icoopid])
        # re-order facotr levels
    std <- levels(this_station_data$standard)[c(1,2,4,3,5,6,8,7,9,10)]
    this_station_data$standard <- factor(this_station_data$standard, levels=std)
    this_station <- paste(simpleCap(this_station_data$station_name[1]), ', ', this_station_data$state[1],sep='')
    
    o <- order(ddply(this_station_data, .(sldmname), summarize, mean=mean(roc))$mean)
    roc_ord <- levels(this_station_data$sldmname)[o]
    this_station_data$sldmname <- factor(this_station_data$sldmname, levels=roc_ord)
#browser()
    ###################################
    #
    # Bar chart
    #
    ###################################
    f_bar <- paste(u_coopid[icoopid], '_bar', '.pdf',sep='')
    pdf(file.path(plot_dir,f_bar), width=17,height=11, family='Times')
    dev_bar <- dev.cur()

        p_bar <- ggplot(this_station_data,aes(sldmname,tpv,fill=standard)) + facet_grid(soil_inf~.) +
            theme_bw() +scale_fill_brewer(palette="Spectral") +
            geom_bar(stat = "identity",position='dodge', drop=FALSE) + 
            opts(axis.text.x=theme_text(size=8,angle=-90,hjust=0),axis.text.y=theme_text(size=8, hjust=1)) +
            scale_y_continuous(labels=comma) 
            #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
            #    labels = trans_format("log10", math_format(10^.x)), expand=c(0,0)) + coord_cartesian(ylim = r) #+
        p_bar <- p_bar + xlab('Standard Land Development Model (SLDM) (ordered by runoff coefficient)') + ylab('Total Present Value ($)') +
            opts(title=paste('Cost to Comply:',this_station, sep=''))  + 
            opts(strip.text.y = theme_text(size = 7,angle=-90))
        print(p_bar)
    dev.off(dev_bar)

    if(!isTRUE(bar_only)){
    ###################################
    #
    # Heat map
    #
    ###################################
    f_heat <- paste(u_coopid[icoopid],'_heat', '.pdf',sep='')
    pdf(file.path(plot_dir,f_heat), width=17,height=11, family='Times')
    dev_heat <- dev.cur()

               # Cost heat map
        dev.set(dev_heat)
        p_heat <- ggplot(this_station_data,aes(y=sldmname,x=standard,fill=tpv))+
            geom_tile()+facet_grid(~soil_inf)+theme_bw()+
            scale_fill_continuous(labels=comma)+
            #scale_fill_continuous(limits=log10(r),labels=math_format(10^.x),name='TPV ($)')+
            opts(axis.text.x=theme_text(angle=-90,hjust=0))+  
            ylab('Frequency') + xlab('Standard') +
            opts(title=paste('Cost to Comply\n',this_station,sep=''))
        print(p_heat)

    dev.off(dev_heat)

    ###################################
    #
    # Stacked Density 
    #
    ###################################
    f_stack <- paste(u_coopid[icoopid],'_stacked_density', '.pdf',sep='')
    pdf(file.path(plot_dir,f_stack), width=11,height=17, family='Times')
    dev_stack <- dev.cur()

        options(warn=-1)
        p_stack <- ggplot(this_station_data,aes(fill=standard,x=tpv))+ 
            geom_density(position='stack',na.rm=TRUE) + facet_grid(soil_inf~.) + #xlim(4,8) +ylim(0,20) +
            scale_x_continuous(limits=r,labels=math_format(10^.x)) +
            scale_fill_brewer(palette="Spectral") + theme_bw() +
            ylab('Probability Density') + xlab('Log10 of Total Present Value ($)') +
            opts(title=paste('Cost to Comply\n',this_station, sep=''))
        print(p_stack)
        options(warn=0)

    dev.off(dev_stack)


    ###################################
    #
    # Stacked histogram
    #
    ###################################
    f_hist <- paste(u_coopid[icoopid],'_stacked_hist', '.pdf',sep='')
    pdf(file.path(plot_dir,f_hist), width=17,height=11, family='Times')
    dev_hist <- dev.cur()

        p_hist <- ggplot(this_station_data,aes(group=standard,fill=standard,x=tpv)) +
            geom_histogram(binwidth=.2) + facet_grid(~soil_inf) + theme_bw() +
            #scale_x_continuous(limits=r,labels=math_format(10^.x)) + 
            scale_x_continuous(labels=comma)+
            scale_fill_brewer(palette="Spectral") + 
            ylab('Frequency') + xlab('Log 10 of Total Present Value ($)') +
            opts(title=paste('Cost to Comply\n',this_station,sep=''))
        print(p_hist)

    dev.off(dev_hist)

    ###################################
    #
    # Boxplots
    #
    ###################################
    f_box <- paste(u_coopid[icoopid],'_boxplot', '.pdf',sep='')
    pdf(file.path(plot_dir,f_box), width=17,height=11, family='Times')
    dev_box <- dev.cur()

        p_box <- ggplot(this_station_data,aes(group=standard,fill=standard)) +
            geom_boxplot(binwidth=.1,na.rm=T)+facet_grid(~soil_inf)+theme_bw() + aes(x=standard,y=tpv)+ scale_y_continuous(labels=comma)+
            opts(axis.text.x=theme_text(angle=-90,hjust=0))+scale_fill_brewer(palette="Spectral")+ 
            xlab('Standard') + ylab('Log 10 of Total Present Value ($)') +
            opts(title=paste('Cost to Comply\n',this_station,sep=''))
        print(p_box)

    dev.off(dev_box)

    ###################################
    #
    # cross tabulations (contingency tables)
    #
    ###################################
    fn_tables <- file.path(table_dir,paste(u_coopid[icoopid],'_tables', '.xlsx',sep=''))
    xtab <- with(this_station_data,table(sldmname,standard))
    write.xlsx(xtab,file=fn_tables, sheet="Xtab SLDM vs. Standard", append=FALSE)

        # open the density plot
    f_dens <- paste(u_coopid[icoopid],'_density', '.pdf',sep='')
    pdf(file.path(plot_dir,f_dens), width=17,height=11, family='Times')
    dev_dens <- dev.cur()

    s_count <- 0
    for(isoiltype in 1:length(u_soiltype)){
        for(iinfcond in 1:length(u_infcond)){

            s_count = s_count + 1
            count = count + 1

                # get the data just for the the selected conditions
                # groups will include sldm and standards
            this_data <- subset(res,!is.na(tpv) & coopid==u_coopid[icoopid] & soiltype == u_soiltype[isoiltype] & infcond == u_infcond[iinfcond])
            if(nrow(this_data) == 0) next

                # nicely formatted names
            this_soil <- paste(u_soiltype[isoiltype], ' (',u_infcond[iinfcond],')',sep='')

            ###################################
            #
            # cross tabulations, contingency tables
            #
            ###################################
            this_tab_data <- dcast(this_data, sldmname~standard, value.var='tpv')
            write.xlsx(this_tab_data,file=fn_tables, sheet=this_soil, append=TRUE)

            this_xtab <- with(this_data,table(sldmname,standard))
            write.xlsx(this_xtab,file=fn_tables, sheet=paste("Xtab SLDM vs. Standard",this_soil), append=TRUE)

            ###################################
            #
            # Panel Density plot
            #
            ###################################
            dev.set(dev_dens)
            options(warn=-1)
            p_dens <-  ggplot(this_data, aes(x=log10(tpv),group=standard,fill=standard,fill=NA)) + geom_density() + 
                scale_x_continuous(limits=log10(r),labels=math_format(10^.x)) + 
                facet_wrap(~standard)+ ylim(0,3) + scale_fill_brewer(palette="Spectral") + theme_bw() + 
                xlab('Total Present Value ($)') + ylab('Probability Density') +
                opts(title=paste('Cost to Comply\n',this_station, ': ', this_soil, sep=''))
            options(warn=0)
            print(p_dens)
        }
    }
    dev.off(dev_dens);
    } # bar_only
    setTxtProgressBar(pb,icoopid)
}

close(pb)