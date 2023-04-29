suppressMessages({
	require(readr)
	require(dplyr)
	require(tidyr)
})

read_halo<-function(hfile) {

	di=read_csv(hfile,show_col_types=F) %>%
		mutate(SID=gsub(".* S19","S19",`Image Location`)%>%gsub(" .*","",.)) %>% 
		rename(Region=`Classifier Label`) %>% 
		select(SID,Region,ObjectID=`Object Id`,matches(" Positive Classification$|^[XY]"))

	markers=grep(" Positive Classification$",colnames(di),value=T) %>%
		gsub(" Positive Classification","",.)

	di=di %>%
		rename_all(~gsub(" Positive.*","",.)) %>% 
		mutate(x0=(XMax+XMin)/2,y0=-(YMax+YMin)/2) %>%
		gather(Marker,Pos,all_of(markers))

	if(file.exists("meta/markerRename.csv")) {

		markerRename=read_csv("meta/markerRename.csv",show_col_types = FALSE)

		di=left_join(di,markerRename,by=c(Marker="Orig")) %>%
			filter(!is.na(New)) %>%
			mutate(Marker=New) %>%
			select(-New)

		markers=unique(di$Marker)

	}

	di = di %>% mutate(C.UUID=paste0(SID,":",ObjectID)) %>% select(C.UUID,everything())

	list(dat=di,markers=markers)

}