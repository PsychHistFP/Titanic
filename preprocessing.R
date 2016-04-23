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
    return(familymembers)# return familymembers as numeric variable
}

# ## get title
# getTitles = function(namelist) {
#     tmp = vcapply(namelist,
#                  function(string) string[2]
#                  )
# 
#     titles = vcapply(str_split(tmp, ' '),
#                      function(string) string[1]
#                      )
#     return(factor(titles))
# 
# }


mergeTitles = function(title){
  if (title %in% c("Capt.", "Col.", "Major.", "Dr.", "Rev.")) {
    title = "Profession"
  } else if (title %in% c("Don.", "Jonkheer.", "Lady.", "Sir.")) {
    title = "Honor"
  } else if (title %in% c("Mme.", "Mrs.", "Mme.", "Mr.")) {
    title = "Married"
  } else if (title %in% c("Mlle.", "Miss.", "Ms.", "Master.")) {
    title = "Unmarried"
  } else {
    title = "something else"
  }
  return(title)
}


## get handcrafted title 
getTitles = function(namelist) {
  tmp = vcapply(namelist,
                function(string) string[2]
  )
  
  titles = vcapply(str_split(tmp, ' '),
                   function(string) string[1]
  )
  titles = vcapply(titles, mergeTitles)
  return(factor(titles))
  
}



# ## get cabin number
# getCabin = function(data) {
#     cabin = str_sub(data$Cabin, 1, 1)
#     cabin[cabin == ""] = 'unknown'
#     return(factor(cabin))
# }

## get cabin number
getCabin = function(data) {
  cabin = str_sub(data$Cabin, 1, 1)
  cabin[is.na(cabin)] = 'unknown'
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


# ## convert variables to factors and drop others
# cleanData = function(data) {
#     weg = grep('Pass|Name|Ticket|Cabin',names(data))
#     X = data[-weg]
#     factors = grep('Surv|Pclass|Sex|Sib|Parch|Embar', names(X))
#     X[factors] = lapply(X[factors], factor)
#     X[] = lapply(X, imputeVar)
#     return(X)
# }

## convert variables to factors and drop others (hier wurde rumgepfuscht)
cleanData = function(data, test = FALSE) {
  if (test) {
    weg = grep('Name|Ticket|Cabin',names(data))
  } else {
    weg = grep('Pass|Name|Ticket|Cabin',names(data))
  }
  X = data[-weg]
  factors = grep('Surv|Pclass|Sex|Embar', names(X))
  X[factors] = lapply(X[factors], factor)
  return(X)
}
