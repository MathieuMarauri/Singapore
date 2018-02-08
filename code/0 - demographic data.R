
# Get demographics data on proper format. Population density, income and unemploymentrate
# by area. Data comes from:
# http://www.singstat.gov.sg/statistics/browse-by-theme/geographic-distribution

# Inputs: xls files

# Output: table with indicators by area

# Packages --------------------------------------------------------------------------

library('gdata') # read xls files
library('data.table') # dataset manipulation
library('stringi') # string manipulation
library('zoo') # fill na values with last non na
library('rvest') # import table from wikipedia


# Import data -----------------------------------------------------------------------

# First sheet contains information on the subsequent sheets in the file. Extract the sheet
# in a table that will be cleaned later.

# first file
info <- read.xls(xls = 'data/demographic/raw/t7-9.xls', sheet = 1)

# second file
info <- read.xls(xls = 'data/demographic/raw/t143-147.xls', sheet = 1)
# Number of working persons over 15 (estimated)

# clean session
rm(info)

# From wikipedia --------------------------------------------------------------------

# Import population density by area from Wikipedia

population <- 'https://en.wikipedia.org/wiki/Planning_Areas_of_Singapore' %>%
  read_html() %>%
  html_nodes('.wikitable.sortable') %>%
  html_table() %>% 
  .[[1]]


# Age -------------------------------------------------------------------------------

# import data
age <- read.xls(xls = 'data/demographic/raw/t7-9.xls', sheet = 2)
setDT(age)

# remove blank lines
age <- age[X.1 != '']

# remove first column and names columns properly
age[, X := NULL]
names(age) <- as.character(unlist(age[1]))
names(age) <- tolower(stri_replace_all_fixed(str = names(age),
                                             pattern = ' ', 
                                             replacement = '_'))
names(age) <- stri_replace_all_fixed(str = names(age),
                                     pattern = '_-_', 
                                     replacement = '_')

# remove rows with columns names
age <- age[planning_area != 'Planning Area']

# fill planning area with last non empty value
age[planning_area == '', planning_area := NA]
age[, planning_area := na.locf(planning_area)]

# remove rows with total by planning area
age <- age[subzone != 'Total']

# replace - values by NA 
for (j in names(age)) {
  set(x = age, i = which(age[[j]] == '-'), j = j, value = NA)
}

# coerce population to numeric
numeric_cols <- names(age)[-(1:2)]
for (j in numeric_cols) {
  set(x = age, 
      j = j, 
      value = as.numeric(stri_replace_all_fixed(str = age[[j]], 
                                                pattern = ',',
                                                replacement = '')))
}

# save results and clean session
saveRDS(age, 'data/demographic/clean/age.rds')
rm(age, numeric_cols, j)


# Dwelling --------------------------------------------------------------------------

# import data
dwelling <- read.xls(xls = 'data/demographic/raw/t7-9.xls', sheet = 6)
setDT(dwelling)

# remove blank lines
dwelling <- dwelling[X.2 != '' | X.4 == 'Total\nHDB1/']

# remove empty columns and names columns properly
dwelling[, c('X', 'X.1', 'X.12', 'X.13') := NULL]
names(dwelling) <- c('planning_area', 'subzone', 'total', 'hdb_total', 'hdb_1_2_rooms',
                     'hdb_3_rooms', 'hdb_4_rooms', 'hdb_5_rooms', 
                     'condo_miniums_other_apartments', 'landed_properties', 
                     'other_dwelling')

# remove rows with columns names
dwelling <- dwelling[!(subzone %in% c('Subzone', ''))]

# fill planning area with last non empty value
dwelling[planning_area == '', planning_area := NA]
dwelling[, planning_area := na.locf(planning_area)]

# remove rows with total by planning area
dwelling <- dwelling[subzone != 'Total']

# replace - values by NA
for (j in names(dwelling)) {
  set(x = dwelling, i = which(dwelling[[j]] == '-'), j = j, value = NA)
}

# coerce population to numeric
numeric_cols <- names(dwelling)[-(1:2)]
for (j in numeric_cols) {
  set(x = dwelling, 
      j = j, 
      value = as.numeric(stri_replace_all_fixed(str = dwelling[[j]], 
                                                pattern = ',',
                                                replacement = '')))
}

# save results and clean session
saveRDS(dwelling, 'data/demographic/clean/dwelling.rds')
rm(dwelling, numeric_cols, j)


# Datamart subzone ------------------------------------------------------------------

# Create a datamart at the subzone level with population by age and by dwelling.

# import data
age <- readRDS('data/demographic/clean/age.rds')
dwelling <- readRDS('data/demographic/clean/dwelling.rds')

# merge age and dwelling
demographic_subzone <- merge(x = age,
                    y = dwelling,
                    by = c('planning_area', 'subzone'))

# check total is ok
demographic_subzone[total.x != total.y]
demographic_subzone[, total := total.x]
demographic_subzone[, c('total.x', 'total.y') := NULL]

# save results and clean session
saveRDS(demographic_subzone, 'data/demographic/clean/demographic_subzone.rds')
rm(age, dwelling, demographic_subzone)


# Position --------------------------------------------------------------------------

# import data
position <- read.xls(xls = 'data/demographic/raw/t143-147.xls', sheet = 3)
setDT(position)

# remove blank lines
position <- position[X.2 != '']

# remove empty columns and names columns properly
position[, c('X', 'X.1', 'X.12') := NULL]
names(position) <- as.character(unlist(position[1]))
names(position) <- tolower(stri_replace_all_fixed(str = names(position),
                                                  pattern = ' ', 
                                                  replacement = '_'))
names(position)[11] <- 'other_position'

# remove rows with columns names
position <- position[planning_area != 'Planning Area']

# remove row with total
position <- position[planning_area != 'Total']

# coerce population to numeric
numeric_cols <- names(position)[-1]
for (j in numeric_cols) {
  set(x = position, j = j, value = as.numeric(as.character(position[[j]])) * 1000)
}

# save results and clean session
saveRDS(position, 'data/demographic/clean/position.rds')
rm(position, numeric_cols, j)


# Transport time --------------------------------------------------------------------

# import data
transport_time <- read.xls(xls = 'data/demographic/raw/t143-147.xls', sheet = 6)
setDT(transport_time)

# remove blank lines
transport_time <- transport_time[X.2 != '']

# remove empty columns and names columns properly
transport_time[, c('X', 'X.1', 'X.8', 'X.9') := NULL]
names(transport_time) <- as.character(unlist(transport_time[1]))
names(transport_time) <- tolower(stri_replace_all_fixed(str = names(transport_time),
                                                        pattern = ' ', 
                                                        replacement = '_'))
names(transport_time) <- stri_replace_all_fixed(str = names(transport_time),
                                                pattern = '_-_', 
                                                replacement = '_')

# remove rows with columns names
transport_time <- transport_time[planning_area != 'Planning Area']

# remove row with total
transport_time <- transport_time[planning_area != 'Total']

# coerce population to numeric
numeric_cols <- names(transport_time)[-1]
for (j in numeric_cols) {
  set(x = transport_time, 
      j = j, 
      value = as.numeric(as.character(transport_time[[j]])) * 1000)
}

# save results and clean session
saveRDS(transport_time, 'data/demographic/clean/transport_time.rds')
rm(transport_time, numeric_cols, j)


# Transport type --------------------------------------------------------------------

# import data
transport_type <- read.xls(xls = 'data/demographic/raw/t143-147.xls', sheet = 5)
setDT(transport_type)

# remove blank lines
transport_type <- transport_type[X.2 != '']

# remove empty columns and names columns properly
transport_type[, c('X', 'X.1', 'X.14', 'X.15', 'X.16', 'X.17', 'X.18') := NULL]
names(transport_type) <- as.character(unlist(transport_type[1]))
names(transport_type) <- tolower(stri_replace_all_fixed(str = names(transport_type),
                                                        pattern = ' ', 
                                                        replacement = '_'))
names(transport_type)[12] <- 'other_transport_type'

# remove rows with columns names
transport_type <- transport_type[planning_area != 'Planning Area']

# remove row with total
transport_type <- transport_type[planning_area != 'Total']

# coerce population to numeric
numeric_cols <- names(transport_type)[-1]
for (j in numeric_cols) {
  set(x = transport_type, 
      j = j, 
      value = as.numeric(as.character(transport_type[[j]])) * 1000)
}

# save results and clean session
saveRDS(transport_type, 'data/demographic/clean/transport_type.rds')
rm(transport_type, numeric_cols, j)


# Industry --------------------------------------------------------------------------

# import data
industry <- read.xls(xls = 'data/demographic/raw/t143-147.xls', sheet = 2)
setDT(industry)

# write table to csv to clean it with excel
# fwrite(industry, 'data/demographic/raw/industry.csv')

# import clean table
industry <- fread(file = 'data/demographic/raw/industry.csv')

# remove blank lines
industry <- industry[X.2 != '']

# remove empty columns and names columns properly
industry[, c('X', 'X.1', 'X.18', 'X.19') := NULL]
names(industry) <- as.character(unlist(industry[1]))
names(industry) <- tolower(stri_replace_all_fixed(str = names(industry),
                                                  pattern = ' ', 
                                                  replacement = '_'))

# remove rows with columns names
industry <- industry[planning_area != 'Planning Area']

# remove row with total
industry <- industry[planning_area != 'Total']

# coerce population to numeric
numeric_cols <- names(industry)[-1]
for (j in numeric_cols) {
  set(x = industry, 
      j = j, 
      value = as.numeric(as.character(industry[[j]])) * 1000)
}

# save results and clean session
saveRDS(industry, 'data/demographic/clean/industry.rds')
rm(industry, numeric_cols, j)


# Revenue ---------------------------------------------------------------------------

# import data
revenue <- read.xls(xls = 'data/demographic/raw/t143-147.xls', sheet = 4)
setDT(revenue)

# write table to csv to clean it with excel
# fwrite(revenue, 'data/demographic/raw/revenue.csv')

# import clean table
revenue <- fread(file = 'data/demographic/raw/revenue.csv')

# remove blank lines
revenue <- revenue[X.2 != '']

# remove empty columns and names columns properly
revenue[, c('X', 'X.1') := NULL]
names(revenue) <- as.character(unlist(revenue[1]))
names(revenue) <- tolower(stri_replace_all_fixed(str = names(revenue),
                                                 pattern = ' ', 
                                                 replacement = '_'))

# remove rows with columns names
revenue <- revenue[planning_area != 'Planning Area']

# remove row with total
revenue <- revenue[planning_area != 'Total']

# coerce population to numeric
numeric_cols <- names(revenue)[-1]
for (j in numeric_cols) {
  set(x = revenue, 
      j = j, 
      value = as.numeric(as.character(revenue[[j]])) * 1000)
}

# save results and clean session
saveRDS(revenue, 'data/demographic/clean/revenue.rds')
rm(revenue, numeric_cols, j)


# Datamart --------------------------------------------------------------------------

# Merge all datasets at the area level. Population by age and dwelling is grouped by area.

# import and group population dataset
demographic_subzone <- readRDS('data/demographic/clean/demographic_subzone.rds')
demographic_subzone[, subzone := NULL]
demographic_subzone[, planning_area := as.character(planning_area)]
demographic_subzone <- demographic_subzone[, lapply(X = .SD, FUN = sum, na.rm = TRUE), 
                                           by = planning_area]

# import data
industry <- readRDS('data/demographic/clean/industry.rds')
position <- readRDS('data/demographic/clean/position.rds')
revenue <- readRDS('data/demographic/clean/revenue.rds')
transport_type <- readRDS('data/demographic/clean/transport_type.rds')
transport_time <- readRDS('data/demographic/clean/transport_time.rds')

# check total are equal
all.equal(industry$total, transport_type$total)

# remove total columns
industry[, total := NULL]
position[, total := NULL]
revenue[, total := NULL]
transport_type[, total := NULL]
transport_time[, total := NULL]

# merge function
mymerge <- function(x, y) {
  merge(x = x, 
        y = y,
        by = 'planning_area',
        all = TRUE)
}

# merge all tables
demographic <- Reduce(mymerge, list(industry, demographic_subzone, position, revenue, 
                                    transport_time, transport_type))

# save results and clean session
saveRDS(demographic, 'data/demographic/clean/demographic.rds')
rm(demographic, industry, demographic_subzone, position, revenue, transport_time, 
   transport_type, mymerge)
