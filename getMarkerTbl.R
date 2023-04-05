require(tidyverse)
source("read_halo.R")

hfiles=fs::dir_ls("FMT multiplex analysis",recurs=T,regex="ObjectData.*.csv.gz")
oo=map(hfiles,read_halo)
tibble(Orig=oo[[1]]$markers) %>% 
	mutate(New=Orig) %>% 
	write_csv("markerRename.csv")
