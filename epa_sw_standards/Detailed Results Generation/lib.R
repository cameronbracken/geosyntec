
    # define : to concatenate strings
":" <- function(...) UseMethod(":") 
":.default" <- .Primitive(":") 
":.character" <- function(...) paste(...,sep="") 

print_flush <- function(...){

    cat(..., '\n')
    flush.console()
    invisible()
}

gce <- function(list, name, n = 1){
    #get character element
    require(stringr)
    list[[name]]
}

gne <- function(list, name, n = 1){
    #get character element
    as.numeric(list[[name]][n])
}

write_csv_e <- function(x,con, end=FALSE){

    if(end)
        cat(x,'\n',file=con, sep='')
    else
        cat(x,',',file=con, sep='')

}