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

getcompletecases <- function(dataframe) {
    # return amount of complete cases in "dataframe"
    completecases <- complete.cases(dataframe)
    dataframe[completecases,]
}

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    totalcases <- data.frame(id = numeric(0), nobs = numeric(0))
    for (i in id) {
        pollution_table <- readpollutionfile(directory, i)
        completecases <- getcompletecases(pollution_table)
        totalcases <- rbind(totalcases, data.frame(id = i, nobs = nrow(completecases)))
    }
        
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    totalcases
}
