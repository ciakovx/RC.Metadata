
setwd("T:/RC.Metadata")
source(file.path("./RC.Metadata", "ScrapeDSpace.R"))


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
  community.df <- read.csv(file=paste0(getwd(), "/data", "/ExportedMetadata/Public", "/10106-", community, ".csv"), na.strings="") 
  
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
  if(any(names(merged.df) == "dc.type..")){
    type <- merged.df$dc..
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



ed.url.str <- url.str(11235, 51)
ed.titles <- titles.urls(ed.url.str)
ed.months.stats <- months.stats(ed.url.str)
# Graphing
ed.merged <- metadata.merge(ed.titles, 11235)
ed.merged.cl <- community.metadata.clean(ed.merged)
stats.barplot.merged(ed.merged.cl)
stats.barplot.merged(ed.merged.cl, top = 10)
stats.barplot.merged(ed.merged.cl, fill.value = "year")
stats.barplot.merged(ed.merged.cl, fill.value = "year", top = 10)
stats.barplot.merged(ed.merged.cl, fill.value = "author", top = 10)
stats.barplot.merged(ed.merged.cl, fill.value = "type", top = 10)

phys.url.str <- url.str(4972, 93)
phys.titles <- titles.urls(phys.url.str)
phys.merged <- metadata.merge(phys.titles, 4972)
phys.merged.cl <- community.metadata.clean(phys.merged)
stats.barplot.merged(phys.merged.cl)
stats.barplot.merged(phys.merged.cl, "year")
stats.barplot.merged(phys.merged.cl, "year", 10)
stats.barplot.merged(phys.merged.cl, "author", 10)
stats.barplot.merged(phys.merged.cl, "type", 10)

elec.url.str <- url.str(4968, 75)
elec.titles <- titles.urls(elec.url.str)
elec.merged <- metadata.merge(phys.titles, 4968)
elec.merged.cl <- community.metadata.clean(elec.merged)
stats.barplot.titles(elec.titles)
stats.barplot.merged(elec.merged.cl)

spco.url.str <- url.str(5194, 131)
spco.titles <- titles.urls(spco.url.str)
stats.barplot.titles(spco.titles)

nursing.url.str <- url.str(1971, 54)
nursing.titles <- titles.urls(nursing.url.str)
nursing.merged <- metadata.merge(nursing.titles, 1971)  
nursing.merged.cl <- community.metadata.clean(nursing.merged)
stats.barplot.titles(nursing.merged)
stats.barplot.merged(nursing.merged.cl, "year", 20)
stats.barplot.merged(nursing.merged.cl, "author", 10)

etd.url.str <- url.str(2, 2759)
etd.titles <- titles.urls(etd.url.str)
stats.barplot.titles(etd.titles, top = 20)
visit.cor(etd.titles)

stats.barplot(etd50)


