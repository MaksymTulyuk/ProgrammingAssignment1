readpollutionfile <- function(directory, number) {
    ## load table from "directory" with the file with "number"
    if (number > 99) {
        fileindex = as.character(number)
    } else if (number > 9) {
        fileindex = paste0("0", number)
    } else {
           fileindex = paste0("00", number)
    }
    read.csv(paste0(directory, "/", fileindex, ".csv"))
}

getcolumnnumber <- function(pollutant) {
    ## get column number depends on "pollutant"
    ## rewrite via table
    if ( pollutant == "sulfate" ) {
        2
    } else if ( pollutant == "nitrate" ) {
        3
    } else {
        NA
    }
}

getcolumnvalues <- function(dataframe, column) {
    # return column values as list without NA
    as.list(dataframe[!is.na(dataframe[,column]),column])
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## list.files(directory)
    column <- getcolumnnumber(pollutant)
    allvalues <- list()
    for (i in id) {
        pollution_table <- readpollutionfile(directory, i)
        valuefromfile <- getcolumnvalues(pollution_table, column)
        allvalues <- append(allvalues, valuefromfile)
    }
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    mean(as.numeric(allvalues))
}
