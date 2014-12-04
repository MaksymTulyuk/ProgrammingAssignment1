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

getcolumnvalues <- function(dataframe, column) {
    # return column values as list without NA
    as.list(dataframe[!is.na(dataframe[,column]),column])
}

getcompletecases <- function(dataframe) {
    # return amount of complete cases in "dataframe"
    completecases <- complete.cases(dataframe)
    dataframe[completecases,]
}

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    corperlocation <- vector()
    for (i in 1:332) {
        pollution_table <- readpollutionfile(directory, i)
        completecases <- getcompletecases(pollution_table)
        if (nrow(completecases) > threshold) {
            corlocation <- cor(completecases[["sulfate"]], completecases[["nitrate"]])
            corperlocation <- append(corperlocation, corlocation)
        }
    }
    
    ## Return a numeric vector of correlations
    corperlocation
}
