## The purpose of these functions is to retrieve statistics from the records of individual items on DSpace and
## visualize them dynamically. DSpace does not provide a flexible or dynamic way to retrieve or view statistics,
## but these stats can be useful ways of seeing which items in a repository are accessed most. 


# Import packages
library(XML)
library(ggplot2)
library(stringr)

# Set your working directory to the folder including this code and data. 
# Example: setwd("your directory/RC.Metadata")
setwd("T:/RC.Metadata")

#####################################################################################################################
## community.data
##
## The community.data function gathers data on the community level from the file names, manually retrieving the total
## number of items, and by scraping the community name. It requires a directory of metadata exported from DSpace, each
## saved as CSV files.
##
## Argument:
##    The directory in which the exported metadata is, assuming those files are named according to the convention
##      10106-XXXX (number of X does not matter)
##
## Returns:
##    A dataframe with the following fields:
##      filename: name of file of exported metadata    
##      handle: community handle
##      number.of.items: total number of items in the community
##      url.base: the base URL to the browse by title for each community
##      community: the community name, as scraped from the url.base
community.data <- function(directory){
  all.files <- list.files(directory)
  
  # Start with the seventh character and take off the last five to get the community handle
  handle <- sapply(all.files, function(z) as.numeric(str_sub(z, start=7, end = -5)))
  
  # These totals were manually derived. The total is generated on the fly and appears as "Now showing items 1-20 of 114."
  # I could have done some string manipulation but decided simply to retrieve the total number of items manually.
   number.of.items <- c(3156, 52, 3, 13, 89, 140,
                       150, 104, 395, 90, 5, 54, 10,
                       9, 15, 1, 1, 75,
                       93, 261, 131, 102, 3, 2186,
                       4, 6025, 72, 114)
  
  # Get the path to the community browse-by-title page
  url.base <- paste0("http://dspace.uta.edu/handle/10106/", 
                     handle, 
                     "/browse?rpp=20&etal=-1&type=title&sort_by=1&order=ASC&offset=")
  
  # Parse the url for each url.base and grab the community name. If it can't get to the page, it will return NULL values,
  # which cannot be unlisted. So NULL are converted to NA before unlisting.
  community <- sapply(url.base, 
                      function(z) {
                        html <- htmlTreeParse(z, useInternalNodes=T)
                        community <- xpathSApply(html, "//*[@id='ds-trail']/li[2]/a", xmlValue)
                      })
  community[unlist(lapply(community, is.null))] <- NA
  final <- data.frame("filename" = all.files, 
                      "handle" = unlist(handle),
                      number.of.items,
                      url.base,
                      "community" = unlist(community),
                      stringsAsFactors = FALSE)
  return(final)
}
#####################################################################################################################


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
  df <- data.frame(matrix(nrow=0, ncol=4))
  names(df) <- c("title", "url", "id", "community")
  
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
    community <- xpathSApply(html, "//*[@id='ds-trail']/li[2]/a", xmlValue)
    x.handle <- data.frame("x.handle" = as.numeric(str_sub(dc.identifier.uri, start=15)))
    x <- data.frame("title" = dc.title, 
                    "url" = dc.identifier.uri,
                    "x.handle" = x.handle,
                    "community" = community)
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
## months.stats
##
## We had some incorrect statistics for April, May, June, July, and August 2014. This loop will collect those
## statistics and return them to a dataframe. They can then be tallied and subtracted from the final.
## This will only work for April-August. If this function is run in the future, it will collect the last five months
## of statistics but assign them under April-August (although it scrapes the month names, it only does to to check that
## they are correct. The month names are manually placed into the final dataframe.)

# Create an emply dataframe of three columns and rename those columns.
months.stats <- function(z){
  # Create an emply dataframe of three columns and rename those columns.
  df <- data.frame(matrix(nrow = 0, ncol = 4))
  names(df) <- c("title", "url", "id", "community")
  
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
    community <- xpathSApply(html, "//*[@id='ds-trail']/li[2]/a", xmlValue)
    x <- data.frame("title" = dc.title, 
                    "url" = dc.identifier.uri,
                    "x.handle" = x.handle,
                    "community" = community)
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
    
    #  If the item has zero total visits, it will appear as an empty list. 
    # This loop says that if it's not a list (meaning, if it has view statistics), 
    # scrape the stats page to collect the last five months of statistics. First it scrapes the months names and
    # checks that they're right. It will throw an error if there are any NA values, if they are different, or if 
    # they are blank (i.e. have no views in the last six months. If they don't match or are blank,
    # it will put NA values for each month. If they do match, grab the values corresponding to those months (the 6-10 values
    # in the table) and put them in the dataframe.
    if(!is.list(stats)){ 
      title <- xpathSApply(perm.tmp, "//*[@id='aspect_statistics_StatisticsTransformer_cell_01'][1]", xmlValue)
      month.names <- xpathSApply(perm.tmp, "//*[@class='ds-table-row odd']/td", xmlValue)
      anomalies.names <- month.names[c(6:10)]
      if(is.na(all(anomalies.names != c("April 2014", "May 2014", "June 2014", "July 2014", "August 2014")))){
        m <- rbind(m, NA)
      } else {
        if(all(anomalies.names != c("April 2014", "May 2014", "June 2014", "July 2014", "August 2014"))) {
          warning(paste("The months for", title, "in", community, "are either blank or are not April-August 2014"))
          m <- rbind(m, NA)
          } else {
            month.views <- xpathSApply(perm.tmp, "//*[@class='ds-table-row even']/td", xmlValue)
            anomalies.views <- rbind(as.numeric(month.views[c(6:10)]))
            m <- rbind(m, anomalies.views)
          }
      }
      # If the item has zero total visits, it will appear as an empty list. 
      # This loop says that if it's not a list (meaning, if it has view statistics), retrieve the total visits (the first item in the list)
      # Sometimes there will be total visits but not file visits. If that is the case, file visits is NA. If not, retrieve the second item in the list.
      # If the item is an empty list, put an NA value (it has no views)
      # put the views. If it is, put NA.
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
  names(m) <- c("April 2014", "May 2014", "June 2014", "July 2014", "August 2014")
  names(n) <- c("total.visits", "file.visits")
  final.anomalies <- cbind(df, n, m)
  return(final.anomalies)
}







#####################################################################################################################
## stats.barplot.titles
##
## stats.barplot.titles prints a barplot of file visits for all items in a community from the dataframe returned from the 
## titles.urls function. Use this if you do not have access to the metadata, and merely have run the titles.url function to scrape the data.
## This function does not handle labels well for communities that have items with exactly the same title.
## That will need to be fixed in future implementations.
##
## Arguments:
##    titles.df: Data frame. Returned from titles.urls function
##    top: Numeric. Optional. A value to limit the number of items graphed. e.g. if you want to see the top ten items, enter 10.
##
## Returns:
##     A horizontal barplot of all titles in a community, ordered by file visits, and labeled with quantities
stats.barplot.titles <- function(titles.df, top){
  # Sort title by number of file visits and eliminate titles for which there are no (NA) visits
  titles.df$title <- str_wrap(titles.df$title, width = 50)
  titles.df$views.ordered <- reorder(titles.df$title, titles.df$file.visits) 
  titles.df <- titles.df[complete.cases(titles.df$file.visits), ]
  community.name <- as.character(titles.df$community[1])
  
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
    ggtitle(label=paste("Total Downloads Per Item for", community.name)) +
    scale_y_continuous(expand = c(0.1, 0)) +
    ylab("Downloads") +
    xlab("Item") +
    theme_bw()
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
  titles.df <- titles.df[complete.cases(titles.df$file.visits), ]
  r <- cor(titles.df$total.visits, titles.df$file.visits) # Get correlation of variables
  community.name <- titles.df$community[1]
  graph <- ggplot(data=titles.df) +
    aes(total.visits, file.visits) +
    geom_point() +
    xlab("Total Visits to Site Page") +
    ylab("File Downloads") +
    ggtitle(paste("Correlation between page visits and file downloads for", community.name)) +
    theme_bw() + # Use bw theme
    geom_smooth(aes(total.visits, file.visits), method=lm, se=FALSE) +  # Add linear regression line
    geom_text(label=paste("r =", r), x=-Inf, y=Inf, hjust=-0.2, vjust=1.2) # Add label for r in upper lft corner
  print(graph)
}
#####################################################################################################################





##################################################
# Snippet of code gathered from http://beckmw.wordpress.com/2013/04/15/how-long-is-the-average-dissertation/




# For future reference, some scraping strategies. I decided not to scrape this data but rather 
# to join my df later with the metadata. Keeping it here for future reference if the
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