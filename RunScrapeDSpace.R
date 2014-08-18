## The purpose of these functions is to run functions sourced from ScrapeDSpace. There are actual functions that loop
## through files saved in the RC.Metadata/data folder, and also one liners used to demo the code from within RStudio.
setwd("T:/RC.Metadata")
source(file.path("./RC.Metadata", "ScrapeDSpace.R"))



#####################################################################################################################
## get.all.titles
##
## The get.all function uses the data generated from the community.data function as variables in the
## url.str and titles.stats functions to gather the statistics data and return a data frame. 
## It requires ScrapeDSpace.R to be sourced, and community.data to be run first. The date in the write.csv command
## should be changed each time this function is run so as to not overwrite previous data.
##
## Argument:
##    comm.data: the community data function returned to "all.community.data"
##
## Returns:
##    CSVs for each community, saved in the specified directory. These CSVs will be joined with data from
##    the community.data function and variables include:
##      community: name of community (scraped from page in community.data function)
##      title: item title
##      url: Item URL (/handle/10106/itemhandle)
##      x.handle: Item handle. Used 
##      total.visits: Total view statistics for the item page
##      file.visits: Total view statistics for the item bitstream (downloads)
all.community.data <- community.data("T:/RC.Metadata/data/ExportedMetadata/Public")
get.all.titles <- function(comm.data){
  all.community.data <- comm.data
  for(i in 1:nrow(all.community.data)){
    local.url.str <- url.str(all.community.data[i, 2], all.community.data[i, 3])
    local.titles.urls <- titles.urls(local.url.str)
    final <- data.frame("community" = rep(all.community.data$community[i], nrow(local.titles.urls)), 
                        local.titles.urls)
    write.csv(final, file = paste0("T:/RC.Metadata/results/CHANGE.THIS.DATE/tables/view.statistics"), row.names = FALSE)
  }
}

# Example:
get.all.titles(all.community.data)
#####################################################################################################################

#####################################################################################################################
## get.all.months
##
## The get.all.months function uses the data generated from the community.data function as variables in the
## url.str and months.stats functions to gather the statistics data and return a data frame. ONLY use this function
## to return statistics from April-August 2014. Otherwise use the get.all function.
## It requires ScrapeDSpace.R to be sourced, and community.data to be run first.The date in the write.csv command
## should be changed each time this function is run so as to not overwrite previous data.
##
## Argument:
##    comm.data: the community data function returned to "all.community.data"
##
## Returns:
##    CSVs for each community, saved in the specified directory. These CSVs will be joined with data from
##    the community.data function and variables include:
##      community: name of community (scraped from page in community.data function)
##      title: item title
##      url: Item URL (/handle/10106/itemhandle)
##      x.handle: Item handle. Used 
##      total.visits: Total view statistics for the item page
##      file.visits: Total view statistics for the item bitstream (downloads)
##      April.2014: Total view statistics for the item page in April
##      May.2014: Total view statistics for the item page in May
##      June.2014: Total view statistics for the item page in June
##      July.2014: Total view statistics for the item page in July
##      August.2014: Total view statistics for the item page in August
all.community.data <- community.data("T:/RC.Metadata/data/ExportedMetadata/Public")
get.all.months <- function(get.all.titles.directory){
  for(i in 1:nrow(all.community.data)){
    local.url.str <- url.str(all.community.data[i, 2], all.community.data[i, 3])
    local.months.stats <- months.stats(local.url.str)
    final <- data.frame("community" = rep(all.community.data$community[i], nrow(local.months.stats)), 
                        local.months.stats)
    write.csv(final, file = paste0("./results/CHANGE.THIS.DATE/tables/view.statistics.months/10106-", all.community.data$handle[i], "_statistics", ".csv"), row.names = FALSE)
  }
}

# Example:
get.all.months(all.community.data)
#####################################################################################################################

#####################################################################################################################
## get.all.visit.cor
##
## Uses exported metadata, all.community.data and visits.cor to get a correlation between file visits and total visits for
## each RC community and saves them to a folder. The date in the write.csv command
## should be changed each time this function is run so as to not overwrite previous data.
##
## Argument:
##    comm.data: the community data function returned to "all.community.data"
##
## Returns:
##    Plots for each community saved to the specified directory.
get.all.visit.cor <- function(get.all.titles.directory){
  local.dir <- file.path(getwd(), "results", get.all.titles.directory)
  all.files <- list.files(local.dir)
  for(i in 1:length(all.files)){
    x <- read.csv(file.path(local.dir, all.files[i]))
    visit.cor(x)
    ggsave(paste0("./results/CHANGE.THIS.DATE/plots/visit.cor/", str_sub(all.files[i], end=-16), "_visitcor", ".png"), width=15, height=15)
  }
}

# Example:
get.all.visit.cor("2014-08-18/tables/view.statistics.months")
#####################################################################################################################

#####################################################################################################################
## get.all.stats.barplot.titles
##
## The get.all.stats.barplot.titles function returns barplots for the top.x items in each community.
## The date in the write.csv command
## should be changed each time this function is run so as to not overwrite previous data.
get.all.stats.barplot.titles <- function(get.all.titles.directory, top.x) {
  local.dir <- file.path(getwd(), "results", get.all.titles.directory)
  all.files <- list.files(local.dir)
  for(i in 1:length(all.files)){
    x <- read.csv(file.path(local.dir, all.files[i]))
    stats.barplot.titles(x, top = top.x)
    ggsave(paste0("./results/CHANGE.THIS.DATE/plots/stats.barplot.titles/", str_sub(all.files[i], end=-16), "_barplot.titles", ".png"), width=15, height=15)
  }
}

# Example:
get.all.stats.barplot.titles("2014-08-18/tables/view.statistics.months/", 20)
#####################################################################################################################





#############################################################################
## EXAMPLE FUNCTIONS FROM ScrapeDSpace.R
#############################################################################

ed.url.str <- url.str(11235, 51)
ed.titles <- titles.urls(ed.url.str)
ed.months.stats <- months.stats(ed.url.str)

ed.summary <- df.summary.stats(ed.titles)
# Graphing
visit.cor(ed.titles)
stats.barplot.titles(ed.titles)
stats.barplot.titles(ed.titles, 20)



phys.url.str <- url.str(4972, 93)
phys.titles <- titles.urls(phys.url.str)
phys.summary <- stats.barplot.titles(phys.titles)

elec.url.str <- url.str(4968, 75)
elec.titles <- titles.urls(elec.url.str)
stats.barplot.titles(elec.titles)

spco.url.str <- url.str(5194, 131)
spco.titles <- titles.urls(spco.url.str)
stats.barplot.titles(spco.titles)

nursing.url.str <- url.str(1971, 54)
nursing.titles <- titles.urls(nursing.url.str)
stats.barplot.titles(nursing.merged)

etd.url.str <- url.str(2, 2759)
etd.titles <- titles.urls(etd.url.str)
stats.barplot.titles(etd.titles, top = 20)
visit.cor(etd.titles)

stats.barplot(etd50)


arch.url.str <- url.str(9467, 72)
arch.titles <- titles.urls(arch.url.str)
arch.titles.months <- months.stats(arch.url.str)

k <- list.files("T:/RC.Metadata/results/2014-08-15/tables/view.statistics")
for (i in 1:length(k)){
  x <- read.csv(paste0("T:/RC.Metadata/results/2014-08-15/tables/view.statistics/", k[i]))
  names(x)[1] <- "community"
  names(x)[11] <- "August.2014"
  write.csv(x, file = paste0("T:/RC.Metadata/results/2014-08-15/tables/two/", k[i]), row.names = FALSE)
}
for (i in 1:length(k)){
  x <- read.csv(paste0("T:/RC.Metadata/results/2014-08-15/tables/view.statistics/", k[i]))
  write.csv(x, file = paste0("T:/RC.Metadata/results/2014-08-15/tables/two/", "10106-", k[i]), row.names = FALSE)
}
