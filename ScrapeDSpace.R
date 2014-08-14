## The purpose of these functions is to retrieve statistics from the records of individual items on DSpace and
## visualize them dynamically. DSpace does not provide a flexible or dynamic way to retrieve or view statistics,
## but these stats can be useful ways of seeing which items in a repository are accessed most. 


# Import packages
library(XML)
library(ggplot2)
library(stringr)

# Set your working directory to the folder including this code and data. 
# Example: setwd("T:/RC.Metadata")
#setwd("directory/RC.Metadata")

#####################################################################################################################
## url.str
##
## The purpose of the url.str function is to return a vector of URLs for each page of 20 items in a given community. 
## For example, if a community has 50 items, there will be three pages, and this function will return the URLs for 
## each of those pages.
## 
## Arguments: 
##   handle: Numeric. The handle is a unique identifier for a commuinity and appears at the end of http://dspace.uta.edu/handle/10106/XXXXX
##   no.items: Numeric. The user needs to provide the number of items in the community. 
##            This figure is used to calculate the number of pages. The number of items should appear on the browse titles page
##            for the community. If it says "Now showing items 1-20 of 102" then input 102.
##
## Returns:
##   A character vector of URLS for each page of 20 items
url.str <- function(handle, no.items){
  # Paste together the address, the handle, and the path to the "browse by title page" into a single string.
  # Then return the total number of pages. There are 20 items per page--the floor function is used to round down 
  # because the loop starts with zero.
  url.base <- paste0("http://dspace.uta.edu/handle/10106/", 
                     handle, 
                     "/browse?rpp=20&etal=-1&type=title&sort_by=1&order=ASC&offset=")
  pgs <- floor(no.items / 20)
  
  # Create empty vector url.all to dump page URLs into. Then loop through and add to the string the multiple of 20 
  # per each number of pages (0 * 20), (1 * 20), (2 * 20), etc.
  url.all <- vector("character", length = 0)  
  for(i in 0:pgs){  # Loop through number of pages
    url.in <- paste0(url.base, i * 20)  # Take the url times twenty for the first set
    url.all <- c(url.all, url.in)  # Return to url.all vector
  }
  return(url.all)
}
#####################################################################################################################


#####################################################################################################################
## titles.url
##
## The purpose of the titles.urls function is to scrape item records in the institutional repository  
## in order to collect view statistics.
## Specifically, it loops through each URL provided from the url.str function, parses the html for
## that page, visits each item on the list, and gathers the URI, the title, the handle, total visits, and file visits 
## into a single dataframe.
## 
## Arguments: 
##   z: Character vector. The vector returned from running url.str.
##
## Returns:
##   A dataframe of five fields: 
##     title: Item title
##     url: Item URL (/handle/10106/itemhandle)
##     x.handle: Item handle. Used 
##     total.visits: Total view statistics for the item page
##     file.visits: Total view statistics for the item bitstream (downloads)
titles.urls <- function(z){
  # Create an emply dataframe of three columns and rename those columns.
  df <- data.frame(matrix(nrow=0, ncol=3))
  names(df) <- c("title", "url", "id")
  
  # The first loop parses the first page of the url.str vector. dc.identifier.uri uses the XPath to retrieve the 
  # href tag for permanent handle. dc.title uses the XPath to retrieve item title. It is then converted to utf-8 because some
  # titles have special characters that need to be converted. Finally, the x.handle simply takes every character after the 
  # fifteenth character to return the item's permanent handle, which will be used in merging with exported metadata in metadata.merge function.
  # All three items are collected into a dataframe and bound into the empty dataframe.
  for(i in seq_along(1:length(z))){
    html <- htmlTreeParse(z[i],useInternalNodes=T)  # Parse the html for the first page
    dc.identifier.uri <- xpathSApply(html, "//*[@id=\'aspect_artifactbrowser_ConfigurableBrowse_div_browse-by-title-results\']/ul/li/div/div/a/@href")  
    dc.title <- xpathSApply(html, "//*[@id=\'aspect_artifactbrowser_ConfigurableBrowse_div_browse-by-title-results\']/ul/li/div/div/a/span", xmlValue)  
    dc.title <- iconv(dc.title, "latin1", "utf-8")  
    x.handle <- data.frame("x.handle" = as.numeric(str_sub(dc.identifier.uri, start=15)))
    x <- data.frame("title" = dc.title, 
                    "url" = dc.identifier.uri,
                    "x.handle" = x.handle)
    df <- rbind(df, x)
  }
  
  # Next another new empty dataframe of two columns is created, as is a vector of URLs to the /statistics page for each item
  n <- data.frame(matrix(nrow=0, ncol=2))
  stats.url <- paste0(df$url, "/statistics")
  
  # This loop takes the titles from the above created dataframe df and visits the statistics page, which it parses to collect the stats
  # The stats are returned in a list, with the first value being the total visits and the second value the file visits.
  for(i in seq_along(1:length(df$title))){
    perm.tmp <- htmlTreeParse(paste0('http://dspace.uta.edu',stats.url[i]),useInternalNodes=T)  # Parse statistics page
    stats <- xpathSApply(perm.tmp, "//*[@id='aspect_statistics_StatisticsTransformer_cell_02']", xmlValue)  # XPath to views
    
    # If the item has zero total visits, it will appear as an empty list. 
    # This loop says that if it's not a list (meaning, if it has view statistics), retrieve the total visits (the first item in the list)
    # Sometimes there will be total visits but not file visits. If that is the case, file visits is NA. If not, retrieve the second item in the list.
    # If the item is an empty list, put an NA value (it has no views)
    # put the views. If it is, put NA.
    if(!is.list(stats)){ 
      total.visits <- as.numeric(stats[1])  # Get the first value and convert it to numeric
      if(length(stats) == 4){
        file.visits <- as.numeric(stats[2])
      } else{
        file.visits <- NA
      }
      visits <- c(total.visits, file.visits)
      n <- rbind(n, visits) 
    } else {
      n <- rbind(n, NA)  
    }
  }
  names(n) <- c("total.visits", "file.visits")
  final.visits <- cbind(df, n)
  return(final.visits)
}
#####################################################################################################################

#####################################################################################################################
## wrong.stats
##
## We had some incorrect statistics for April, May, June, and July 2014. This loop will collect those
## statistics and return them to a dataframe. They can then be tallied and subtracted from the final.
## This will only work for April-July. If it needs to be done in the future, the stop message in the final
## if loop needs to be updated.

# Create an emply dataframe of three columns and rename those columns.
wrong.stats <- function(z){
  # Create an emply dataframe of three columns and rename those columns.
  df <- data.frame(matrix(nrow=0, ncol=3))
  names(df) <- c("title", "url", "id")
  
  # The first loop parses the first page of the url.str vector. dc.identifier.uri uses the XPath to retrieve the 
  # href tag for permanent handle. dc.title uses the XPath to retrieve item title. It is then converted to utf-8 because some
  # titles have special characters that need to be converted. Finally, the x.handle simply takes every character after the 
  # fifteenth character to return the item's permanent handle, which will be used in merging with exported metadata in metadata.merge function.
  # All three items are collected into a dataframe and bound into the empty dataframe.
  for(i in seq_along(1:length(z))){
    html <- htmlTreeParse(z[i],useInternalNodes=T)  # Parse the html for the first page
    dc.identifier.uri <- xpathSApply(html, "//*[@id=\'aspect_artifactbrowser_ConfigurableBrowse_div_browse-by-title-results\']/ul/li/div/div/a/@href")  
    dc.title <- xpathSApply(html, "//*[@id=\'aspect_artifactbrowser_ConfigurableBrowse_div_browse-by-title-results\']/ul/li/div/div/a/span", xmlValue)  
    dc.title <- iconv(dc.title, "latin1", "utf-8")  
    x.handle <- data.frame("x.handle" = as.numeric(str_sub(dc.identifier.uri, start=15)))
    x <- data.frame("title" = dc.title, 
                    "url" = dc.identifier.uri,
                    "x.handle" = x.handle)
    df <- rbind(df, x)
  }
  
  # Next another new empty dataframe of two columns is created, as is a vector of URLs to the /statistics page for each item
  m <- data.frame(matrix(nrow=0, ncol=4))
  n <- data.frame(matrix(nrow=0, ncol=2))
  stats.url <- paste0(df$url, "/statistics")
  
  # This loop takes the titles from the above created dataframe df and visits the statistics page, which it parses to collect the stats
  # The stats are returned in a list, with the first value being the total visits and the second value the file visits.
  for(i in seq_along(1:length(df$title))){
    perm.tmp <- htmlTreeParse(paste0('http://dspace.uta.edu',stats.url[i]),useInternalNodes=T)  # Parse statistics page
    stats <- xpathSApply(perm.tmp, "//*[@id='aspect_statistics_StatisticsTransformer_cell_02']", xmlValue)  # XPath to views
    
    # If the item has zero total visits, it will appear as an empty list. 
    # This loop says that if it's not a list (meaning, if it has view statistics), retrieve the total visits (the first item in the list)
    # Sometimes there will be total visits but not file visits. If that is the case, file visits is NA. If not, retrieve the second item in the list.
    # If the item is an empty list, put an NA value (it has no views)
    # put the views. If it is, put NA.
    if(!is.list(stats)){ 
      month.names <- xpathSApply(perm.tmp, "//*[@class='ds-table-row odd']/td", xmlValue)
      anomalies.names <- month.names[c(6, 7, 8, 9)]
      if(all(anomalies.names != c("April 2014", "May 2014", "June 2014", "July 2014"))) stop("Something is wrong with the months")
      month.views <- xpathSApply(perm.tmp, "//*[@class='ds-table-row even']/td", xmlValue)
      anomalies.views <- rbind(as.numeric(month.views[c(6,7,8,9)]))
      m <- rbind(m, anomalies.views)
      
      total.visits <- as.numeric(stats[1])  # Get the first value and convert it to numeric
      if(length(stats) == 4){
        file.visits <- as.numeric(stats[2])
      } else{
        file.visits <- NA
      }
      visits <- c(total.visits, file.visits)
      n <- rbind(n, visits)
    } else {
      m <- rbind(m, NA)
      n <- rbind(n, NA) 
    }
  }
  names(m) <- c("April 2014", "May 2014", "June 2014", "July 2014")
  names(n) <- c("total.visits", "file.visits")
  final.anomalies <- cbind(df, n, m)
  return(final.anomalies)
}





#####################################################################################################################
## metadata.merge
##
## The metadata.merge function merges the metadata as exported from DSpace with the dataframe returned from titles.urls. 
## This function requires you to download the metadata from the community
## from DSpace, with admin privileges. Save the metadata CSV file, with the filename as the community.
## 
## Arguments: 
##   titles.df: Data Frame. Returned from the titles.urls function
##   community: Numeric. The filename of the CSV of metadata for particular subject (e.g. 10106-1147)
##
## Returns:
##   Data frame of total visits, file visits, and full exported metadata merged
metadata.merge <- function(titles.df, community){
  # Read in the CSV for the metadata as exported from ResearchCommons.
  community.df <- read.csv(file=paste0(getwd(), "/data", "/ExportedMetadata", "/10106-", community, ".csv"), na.strings="") 
  
  # These two if loops concatenate columns variously named dc.identifier.uri. and dc.identifier.uri.. (note the periods at the end)
  if(any(names(community.df == "community.df$dc.identifier.uri."))){
    community.uri <- ifelse(is.na(community.df$dc.identifier.uri),
                            as.character(community.df$dc.identifier.uri.),as.character(community.df$dc.identifier.uri))  # Merges the two different uri fields based on NA values
  }
  if(any(names(community.df) == "dc.identifier.uri..")){
    community.uri <- ifelse(is.na(community.df$dc.identifier.uri),
                            as.character(community.df$dc.identifier.uri..),as.character(community.df$dc.identifier.uri))  # Merges the two different uri fields based on NA values    
  }
  
  # Take away the first 29 characters to leave the handle & returns handle as character, bind the handle 
  # vector to the metadata dataframe, and merge the visits dataframe with the metadata. Create a column called "department"
  # that includes the community handle and bind that to the final dataframe.
  y.handle <-  data.frame("y.handle" = as.numeric(str_sub(community.uri, start=29)))  
  community.df <- data.frame(community.df, y.handle)  
  final <- merge(titles.df, community.df, by.x = "x.handle", by.y = "y.handle", all = TRUE)  
  community <- data.frame("community" = rep(as.character(community), nrow(final)))
  final <- data.frame(final, community)
  return(final)
}
#####################################################################################################################


#####################################################################################################################
## community.metadata.clean
##
## The community.metadata.clean function cleans up inconsistent metadata between the the year, author, and 
## type fields in the dataframe returned from the metadata.merge function, and pulls together a cleaned up dataframe for graphing/
## 
## Arguments: 
##   merged.df: Data Frame. Returned from metadata.merge function
##
## Returns:
##   A more cleaned up and consistent metadata data frame, with inconsistent metadata fields merged, and irrelevant fields excluded.
community.metadata.clean <- function(merged.df){
  # # Merges two different "date issued" fields based on NA values, then merges the with the other 
  # "date issued" field, if it exists, based on NA values, and puts it into a field called 'year.'
  # Uses only the first four characters (the year) and converts to factor
  year <- ifelse(is.na(merged.df$dc.date.issued),
                 as.character(merged.df$dc.date.issued..), as.character(merged.df$dc.date.issued))  
  if(any(names(merged.df) == "dc.date.issued.en_US.")){
    year <- ifelse(is.na(year),
                   as.character(merged.df$dc.date.issued.en_US.), as.character(year))
  }
  year <- data.frame("year" = as.factor(str_sub(year, start=0, end=4)))
  
  # Merge two different "author" fields based on NA values, 
  author <- ifelse(is.na(merged.df$dc.contributor.author),
                   as.character(merged.df$dc.contributor.author..), as.character(merged.df$dc.contributor.author))  
  author <- ifelse(is.na(author),
                   as.character(merged.df$dc.contributor.author.en_US.), as.character(author))
  if(any(names(merged.df) == "dc.type.en_US.")){
    type <- merged.df$dc.type.en_US.
  }
  if(any(names(merged.df) == "dc.type.en.")){
    type <- merged.df$dc.type.en.
  }
  
  # Gather the fields together & return them
  community.df.cl <- data.frame( "title" = merged.df$title, 
                                 author, 
                                 year, 
                                 "community" = merged.df$community, 
                                 "publisher" = merged.df$dc.publisher.en_US., 
                                 type,
                                 "file.visits" = merged.df$file.visits, 
                                 "total.visits" = merged.df$total.visits)
  return(community.df.cl)
}
#####################################################################################################################


#####################################################################################################################
## stats.barplot.titles
##
## stats.barplot.titles prints a barplot of file visits for all items in a community from the dataframe returned from the 
## titles.urls function. Use this if you do not have access to the metadata, and merely have run the titles.url function to scrape the data. 
##
## Arguments:
##    titles.df: Data frame. Returned from titles.urls function
##    top: Numeric. Optional. A value to limit the number of items graphed. e.g. if you want to see the top ten items, enter 10.
##
## Returns:
##     A horizontal barplot of all titles in a community, ordered by file visits, and labeled with quantities
stats.barplot.titles <- function(titles.df, top){
  # Sort title by number of file visits and eliminate titles for which there are no (NA) visits
  titles.df$views.ordered <- reorder(titles.df$title, titles.df$file.visits) 
  titles.df <- titles.df[complete.cases(titles.df$file.visits), ]  # 
  
  # If "top" is passed, reorder the df according to visits and take the top first through the value passed to top
  if(!missing(top)){
    titles.df <- titles.df[with(titles.df, order(-file.visits)), ]  
    titles.df <- titles.df[1:top, ]  
  }
  graph <- ggplot(data=titles.df) +
    aes(views.ordered, file.visits) +
    geom_bar(stat="identity") +
    coord_flip() +
    geom_text(aes(views.ordered, file.visits, label=file.visits), hjust = -0.2) + #set numeric labels
    ggtitle(label=paste("Total Downloads Per Item")) +
    ylab("Downloads") +
    xlab("Item") +
    theme_bw()
  print(graph)
}
#####################################################################################################################


#####################################################################################################################
## stats.barplot.merged
##
## The stats.barplot.merged function is only to be used with the dataframe returned from the community.metadata.clean function.
## It prints a barplot of file visits (downloads) from items in a community, and provides the option to color code the titles
## based on a variable in the dataframe: by author, year, publisher, or type.
##
## Arguments:
##    merged.cl.df: Data frame. Returned from community.metadata.clean function
##    fill.value: Character vector. Optional. Fill the barplots based on a value from within the merged.cl.df. Options include:
##      "type"
##      "author"
##      "year"
##      "publisher"
##    top: Numeric. Optional. A value to limit the number of items graphed. e.g. if you want to see the top ten items, enter 10.
##
## Returns:
##    A horizontal barplot of all titles in a community, ordered by file visits, labeled with quantities, and, optionally
##    filled according to a particular facet.
stats.barplot.merged <- function(merged.cl.df, fill.value, top){
  # Sort title by number of file visits & eliminate titles for which there are no (NA) visits
  merged.cl.df$views.ordered <- reorder(merged.cl.df$title, merged.cl.df$file.visits) 
  merged.cl.df <- merged.cl.df[complete.cases(merged.cl.df$file.visits), ]
  
  # If "top" is passed, reorder the df according to visits and take the top first through the value passed to top
  if(!missing(top)){
    merged.cl.df <- merged.cl.df[with(merged.cl.df, order(-file.visits)), ]  
    merged.cl.df <- merged.cl.df[1:top, ] 
  }
  merged.cl.df$fill.value <- merged.cl.df[ , fill.value]
  
  # If fill.value is passed, use that value in the aes call as a fill. Otherwise plot without a fill value.
  if(!missing(fill.value)){
    graph <- ggplot(data=merged.cl.df) +
      aes(views.ordered, file.visits, fill = fill.value) +
      geom_bar(stat="identity") +
      coord_flip() +
      geom_text(aes(views.ordered, file.visits, label=file.visits), hjust = -0.2) + #set text labels
      #ggtitle(label=paste("File Downloads Per Item:", dpt, "ResearchCommons")) +
      ylab("File Visits") +
      xlab("Item") +
      theme_bw() # Get rid of grayscale background
  } else{
    graph <- ggplot(data=merged.cl.df) +
      aes(views.ordered, file.visits) +
      geom_bar(stat="identity") +
      coord_flip() +
      geom_text(aes(views.ordered, file.visits, label=file.visits), hjust = -0.2) + #set text labels
      #ggtitle(label=paste("File Downloads Per Item:", publ, "ResearchCommons")) +
      ylab("File Visits") +
      xlab("Item") +
      theme_bw() # Get rid of grayscale background
  }
  print(graph)
}
#####################################################################################################################

#####################################################################################################################
## df.summary stats
##
## The df.summary stats function collects some summary statistics for file visits (downloads) and returns them as a character
## vector. I didn't do it in any of the above functions, but this could be placed onto the graph using the geom_text function
##
## Argument:
##    titles.df: Data Frame. Returned from the titles.urls function
##
## Returns:
##    Character vector of mean, median, standard deviation, and range.
df.summary.stats <- function(titles.df){
  df <- titles.df[complete.cases(titles.df), ]
  mean.val<-round(mean(df$file.visits))
  med.val<-median(df$file.visits)
  sd.val<-round(sd(df$file.visits))
  rang.val<-range(df$file.visits)
  txt.val<-paste('mean = ',mean.val,'\nmed = ',med.val,'\nsd = ',sd.val,
                 '\nmax = ',rang.val[2],'\nmin = ', rang.val[1],sep='')
  return(txt.val)
}
#####################################################################################################################

#####################################################################################################################
## visit.cor
##
## The visit.cor function plots the correlation between file visits (downloads) and total visits (page visits). 
## One may think that page visits would necessarily be higher, since one may assume that the page would need to be visited in 
## order for the file to be downloaded.
## However, since files can be downloaded directly from Google Scholar, there is no need to visit the page, and since Google
## Scholar is the primary way to discover research literature, file visits are almost always higher than total visits.
## This function is useful to visualize the correlation between the two, and to see if any files have significantly more
## of one than the other.
## 
## Argument:
##    titles.df: Data Frame. Returned from the titles.urls function
##
## Returns:
##    Plot of the correlation between file visits and total visits, with the correlation coefficient printed in the upper left
visit.cor <- function(titles.df){
  df <- df[complete.cases(df$file.visits), ]
  r <- cor(df$total.visits, df$file.visits) # Get correlation of variables
  graph <- ggplot(data=df) +
    aes(total.visits, file.visits) +
    geom_point() +
    xlab("Total Visits to Site Page") +
    ylab("File Downloads") +
    theme_bw() + # Use bw theme
    geom_smooth(aes(total.visits, file.visits), method=lm, se=FALSE) +  # Add linear regression line
    geom_text(label=paste("r =", r), x=-Inf, y=Inf, hjust=-0.2, vjust=1.2) # Add label for r in upper lft corner
  print(graph)
}
#####################################################################################################################





##################################################
# Snippet of code gathered from http://beckmw.wordpress.com/2013/04/15/how-long-is-the-average-dissertation/




# For future reference, some scraping strategies. I decided not to scrape this data but rather to join my df later with the metadata. Keeping it here for future reference if the
# metadata is not available.
#
# This is the less effective way of pulling author/publisher/date info
# dc.author <- xpathSApply(html, "//*[@id=\'aspect_artifactbrowser_ConfigurableBrowse_div_browse-by-title-results\']/ul/li/div/div[2]/span[1]/span[1]", xmlValue)  # XPath to first listed author 
# dc.publisher <- xpathSApply(html, "//*[@id=\'aspect_artifactbrowser_ConfigurableBrowse_div_browse-by-title-results\']/ul/li/div/div[2]/span[2]/span[1]", xmlValue)  # XPath to publisher
# dc.date <- xpathSApply(html, "//*[@id=\'aspect_artifactbrowser_ConfigurableBrowse_div_browse-by-title-results\']/ul/li/div/div[2]/span[2]/span[2]", xmlValue)  # XPath to date
#
# This is the more effective way, but it causes trouble if one of the fiels is blank. 
# dc.publisher <- xpathSApply(html, "//*[@class='publisher']", xmlValue)  # XPath to publisher
# dc.author <- xpathSApply(html, "//*[@class='author']", xmlValue)  # XPath to first listed author 
# dc.date <- xpathSApply(html, "//*[@class='date']", xmlValue)

# Also useful
# department <- deparse(substitute(community))  # Casts function argument as character string