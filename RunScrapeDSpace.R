setwd("T:/RC.Metadata")
source(file.path(".", "code", "ScrapeDSpace.R"))

#############################################################################
## RUNNING FUNCTIONS FROM ScrapeDSpace.R
#############################################################################
ed.url.str <- url.str(11235, 51)
ed.titles <- titles.urls(ed.url.str)
ed.wrong.stats <- wrong.stats(ed.url.str)

ed.merged <- metadata.merge(ed.titles, 11235)
ed.merged.cl <- community.metadata.clean(ed.merged)
ed.summary <- df.summary.stats(ed.titles)
# Graphing
visit.cor(ed.titles)
stats.barplot.titles(ed.titles)
stats.barplot.titles(ed.titles, 10)
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
phys.summary <- stats.barplot.titles(phys.titles)
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










all.community.data <- community.data("T:/RC.Metadata/data/ExportedMetadata")


get.all <- function(comm.data){
  all.community.data <- comm.data
  all.community.data <- all.community.data[complete.cases(all.community.data$number.of.items), ]
  for(i in 1:nrow(all.community.data)){
    local.url.str <- url.str(all.community.data[i, 2], all.community.data[i, 3])
    local.months.stats <- months.stats(local.url.str)
    final <- data.frame("community" = rep(all.community.data$community[i], nrow(local.months.stats)), 
                        local.months.stats)
    write.csv(final, file = paste0("./results/", all.community.data$handle[i], "_statistics", ".csv"), row.names = FALSE)
  }
}

get.all(all.community.data)



url.str(1, )


