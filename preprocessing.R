library(pryr)
library(stringr)


## vapply for function with character(1) output
vcapply = partial(vapply, FUN.VALUE = character(1), USE.NAMES = FALSE)


## get number of passengers with the same name
getFamMembers = function(namelist) {
    famnames = vcapply(namelist,
                       function(string) string[1]
                       )
    count_family = table(famnames)
    familymembers = count_family[famnames]
    return(factor(familymembers))
}

## get title
getTitles = function(namelist) {
    tmp = vcapply(namelist,
                 function(string) string[2]
                 )

    titles = vcapply(str_split(tmp, ' '),
                     function(string) string[1]
                     )
    return(factor(titles))

}

## get cabin number
getCabin = function(data) {
    cabin = str_sub(data$Cabin, 1, 1)
    cabin[cabin == ''] = 'unknown'
    return(factor(cabin))
}

## special ticket yes/no
getTicket = function(data) {
    ticket = !is.na(as.numeric(data$Ticket))
    return(factor(ticket))
}

## impute
imputeVar = function(var) {
    if (is.factor(var)) {
        var[is.na(var)] = names(which.max(table(var)))
    } else {
        var[is.na(var)] = median(var, na.rm = TRUE)
    }
    return(var)
}

## main function to create new features from the string variables
addFeatures = function(data) {
    namelist = str_split(data$Name, ', ')

    ## fam
    fammembers = getFamMembers(namelist)
    ## titles
    titles = getTitles(namelist)
    ## ticket
    specialticket = getTicket(data)
    ## cabin letter
    cabin = getCabin(data)


    newdata = cbind(data, fammembers, titles, specialticket, cabin)
    return(newdata)
}


## convert variables to factors and drop others
cleanData = function(data) {
    weg = grep('Pass|Name|Ticket|Cabin',names(data))
    X = data[-weg]
    factors = grep('Surv|Pclass|Sex|Sib|Parch|Embar', names(X))
    X[factors] = lapply(X[factors], factor)
    X[] = lapply(X, imputeVar)
    return(X)
}

