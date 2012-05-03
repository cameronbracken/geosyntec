library('xlsx')

out_dir <- 'oc_checks'
if(!file.exists(out_dir)) dir.create(out_dir)

    # change the name to csv and added the last column name to each file
    # read in the data
#options(stringsAsFactors = FALSE)
#oc_msa_op <- read.csv('15550_100_ac_SLDM_Feasibility_OC_MSA_OP.csv',row.names=NULL)
#oc_msa_ow <- read.csv('15550_100_ac_SLDM_Feasibility_OC_MSA_OW.csv',row.names=NULL)
#oc_state_ow <- read.csv('15550_100_ac_SLDM_Feasibility_OC_STATE_OW.csv',row.names=NULL)
#oc_state_op <- read.csv('15550_100_ac_SLDM_Feasibility_OC_STATE_OP.csv',row.names=NULL)
#oc_no <- read.csv('15550_100_ac_SLDM_Feasibility_OC_No.csv',row.names=NULL)

oc <- read.csv('OppCostQC1_1.csv')
decreasing <- subset(oc,trend=='DECREASING')$sauid

    # subset the data, just the decreasing cases
oc_msa_op <- subset(oc_msa_op, Standard.Area.Unit.ID %in% decreasing & Cost.Rank.for.Area.Distribution.Options == 1)
oc_msa_ow <- subset(oc_msa_ow, Standard.Area.Unit.ID %in% decreasing & Cost.Rank.for.Area.Distribution.Options == 1)
oc_state_op <- subset(oc_state_op, Standard.Area.Unit.ID %in% decreasing & Cost.Rank.for.Area.Distribution.Options == 1)
oc_state_ow <- subset(oc_state_ow, Standard.Area.Unit.ID %in% decreasing & Cost.Rank.for.Area.Distribution.Options == 1)
oc_no <- subset(oc_no, Standard.Area.Unit.ID %in% decreasing & Cost.Rank.for.Area.Distribution.Options == 1)

summary_table_bmp <- summary_table_tpv <- list()

    # define : to concatenate strings
":" <- function(...) UseMethod(":") 
":.default" <- .Primitive(":") 
":.character" <- function(...) paste(...,sep="") 

oc_type <- c('oc_msa_op','oc_msa_ow','oc_state_ow','oc_state_op','oc_no')

for(k in oc_type){ # oc_{state,msa}_{op_oc}
    
    this_tpv <- this_bmp <- matrix(NA,nrow=nrow(oc_msa_op),ncol=5)

    for(i in 1:nrow(get(k))){
        for(j in 1:5){ # source area
            
            bmpcol1 <- paste("BMP.",j,".1.Family",sep='')
            bmpcol2 <- paste("BMP.",j,".2.Family",sep='')
            tpvcol1 <- paste("BMP.",j,".1.TPV....",sep='')
            tpvcol2 <- paste("BMP.",j,".2.TPV....",sep='')
            bmp1 <- as.character(get(k)[i,bmpcol1])
            bmp2 <- as.character(get(k)[i,bmpcol2])
            tpv1 <- as.numeric(as.character(get(k)[i,tpvcol1]))
            tpv2 <- get(k)[i,tpvcol2]
            this_bmp[i,j] <- bmp1 : ifelse(bmp2=='--','','/' : bmp2)
            this_tpv[i,j] <- tpv1 + ifelse(tpv2=='--',0,as.numeric(as.character(tpv2)))

            #browser()
        }
    }
    this_tpv <- as.data.frame(this_tpv)
    this_bmp <- as.data.frame(this_bmp)
    names(this_tpv) <- names(this_bmp) <- paste("SA",1:5,sep='')
    rownames(this_tpv) <- rownames(this_bmp) <- get(k)[,1]
    summary_table_tpv[[k]] <- this_tpv
    summary_table_bmp[[k]] <- this_bmp
}

for(i in 1:length(oc_type)){

    fn_bmp <- file.path(out_dir, 'oc_bmp.xlsx')
    fn_tpv <- file.path(out_dir, 'oc_tpv.xlsx')
    write.xlsx(summary_table_bmp[[i]],file=fn_bmp,sheet=oc_type[i],append=ifelse(i==1,F,T))
    write.xlsx(summary_table_tpv[[i]],file=fn_tpv,sheet=oc_type[i],append=ifelse(i==1,F,T))

}