#Make specs for audio files
source("https://raw.githubusercontent.com/drwilkins/dynaSpec/adding-rspectVid/R/ggSpec.R")
source("https://raw.githubusercontent.com/drwilkins/dynaSpec/adding-rspectVid/R/isURL_mytheme_helpers.R")
source("https://raw.githubusercontent.com/drwilkins/dynaSpec/adding-rspectVid/R/paged_spectro.R")
source("https://raw.githubusercontent.com/drwilkins/dynaSpec/adding-rspectVid/R/processSound.R")
source("https://raw.githubusercontent.com/drwilkins/dynaSpec/adding-rspectVid/R/prep_static_ggspectro.R")
require(pacman)
p_load(gganimate,ggplot2,av,tidyverse,seewave,tuneR,tools,scales,viridis)

is.url<-function(x){grepl("www\\.|http:|https:", x)}
afilez<-normalizePath(list.files("www/",pattern=".wav",full.names=T) )

# lapply(afilez,function(x) prep_static_ggspectro(x,savePNG=T,destFolder="www",crop=3,xlim=3,onlyPlotSpec=F,bgFlood=T,ampTrans=2,min_dB=-40))

x=afilez[18]
prep_static_ggspectro(x,savePNG=T,destFolder="www",crop=3,onlyPlotSpec=F,bgFlood=T,ampTrans=1.5,min_dB=-30,yLim=c(0,13))

