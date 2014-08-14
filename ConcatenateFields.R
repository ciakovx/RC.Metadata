library(stringr)

setwd("T:/RC.Metadata")

elements <- c("contributor", "coverage", "creator", "date", "description", "format", 
                 "identifier", "language", "publisher", "relation", "rights", "source", 
                 "subject", "title", "type")
dc.elements <- paste0("dc.", elements)
dc.elements.regex <- paste0(dc.elements, ".+")

# Return a list of metadata for each community
all.files <- function(directory){
  # set path to folder with CSV files, list files in directory, and get the path to the files
  dirct <- file.path(getwd(), directory) 
  files <- list.files(dirct) 
  filepath <- file.path(dirct, files) 
  all.files <- lapply(filepath, read.csv, check.names = FALSE, na.strings = "")
  return(all.files)
}
rc.files <- all.files("data")

# Return the full set of tags and extract the unique elements
rc.nm.list <- lapply(rc.files, colnames)  # Tags for each community
rc.nm <- as.character(matrix(unlist(rc.nm.list)))
rc.nm <- unique(rc.nm)  # Unique metadata tags across RC

# Return each element & its variation to a separate vector in a list
zz <- vector("list")
grouped.elements <- function(elements.vec){
  lq <- for(i in 1:length(elements.regex)){
    y <- str_extract(elements.vec, dc.elements.regex[i])
    y <- y[complete.cases(y)]
    zz[[i]] <- y
  }
  return(zz)
}
rc.nm.grp <- grouped.elements(rc.nm.unq)







#Experimenting with looping through each file and collecting all elements into separate items
r <- rc.files[[1]]
r.nm <- colnames(r)
rc.nm.grp1 <- grouped.elements(r.nm)

#Get elements per df. Use this to get an idea of what you need to do for each element.
elms <- function(community, element.number){
    r <- rc.files[[community]]
    grp <- grouped.elements(colnames(r))
    element <- grp[[element.number]]
    count.vec <- vector("numeric")
    for(i in 1:length(element)){
      compl <- as.numeric(table(complete.cases(r[,element[i]]))["TRUE"])
      count.vec <- c(count.vec, compl)
    }
    all <- data.frame(element, count.vec)
    return(all)
}
elms(1, 1)
elms(1, 2)    

#Create indices
element <- grp[[element.number]]
auth <- element[2:4]
f <- r[, auth]
indx <- lapply(f, )


      
      
      #This is some good regex to find dc.contributor out of dc.contributor.advisor, etc.
      qx <- str_extract(q, "(dc.[a-z]*\\.)")
      qx <- qx[1]
      qx1 <- str_replace_all(q, fixed(qx), replacement="")
      
      
      r.in <- grp[[i]] %in% colnames(r)
      r.sbst <- r[]
  }
}

#10106-1





rq <- do.call(rbind.data.frame, lq)

r.nm.elem <- str_extract(colnames(r), "[^.]*")  # Extract everything before the first period
rc.nm.base <- unique(rc.nm.base)  # Get unique tags
rc.nm.each <- str_extract(rc.nm, "[^.]*")


#Concatenate
q <- rc.files[[2]]
nm <- colnames(q)

#contributor
all.files <- lapply(filepath, read.csv, na.strings = "")
rc.nm <- str_sub(colnames(r), 4)  # Eliminate beginning "dc."
rc.nm <- rc.nm[-1:-2]  # Eliminate "id" and "collection"
rc.nm.unq <- str_extract(rc.nm, "[^.]*")  # Extract everything before the first period
rc.nm.unq <- unique(rc.nm.unq)  # Get unique tags

r.contributor <- str_detect(colnames(r), "dc.contributor")
r.contributor <- r[, r.contributor]

r.coverage <- str_detect(colnames(r), "dc.coverage")

r.format <- str_detect(colnames(r), "dc.format")
View(r[, r.format])

index1 <- !is.na(r$dc.format.en_US..)
index2 <- !is.na(r$dc.format.en_US.)
yy <- data.frame(index1, index2)


REGEX /\..*


# This does poorly if two values are in one column
rr <- ifelse(is.na(r$dc.format.en_US..),
       as.character(r$dc.format.en_US.),as.character(r$dc.format.en_US..))  # Merges the two different uri fields based on NA values








  # Read the files in and gather the metadata tags. 
  # check.names is false because otherwise brackets [] are converted to periods ..
  all.files <- lapply(filepath, read.csv, check.names = FALSE, na.strings = "")
  all.tags <- lapply(all.files, colnames)
  tags.list <- lapply(all.tags, 
                      function(x){
                        str_match
                      })
  
  
  
}

# Extract all metadata



x <- all.files[[5]]
y <- sapply(x, complete.cases)

z <- sapply(y, table)
z1 <- do.call(rbind.data.frame, z)