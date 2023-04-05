require(readr)
require(dplyr)

read_halo<-function(hfile) {

	di=read_csv(hfile) %>% 
		mutate(SID=gsub(".* S19","S19",`Image Location`)%>%gsub(" .*","",.)) %>% 
		rename(Region=`Classifier Label`) %>% 
		select(SID,Region,ObjectID=`Object Id`,matches(" Positive Classification$|^[XY]"))

	markers=grep(" Positive Classification$",colnames(di),value=T) %>%
		gsub(" Positive Classification","",.)

	di=di %>%
		rename_all(~gsub(" Positive.*","",.)) %>% 
		mutate(x0=(XMax+XMin)/2,y0=(YMax+YMin)/2)

	list(dat=di,markers=markers)

}