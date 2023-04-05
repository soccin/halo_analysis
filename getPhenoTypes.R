source("read_halo.R")
hfile="raw/FMT multiplex analysis/Halo archive 2023-02-01 12-18 - v3.4.2986/ObjectData/Jon.P. S19-48319 2-1 G1 R_1.ome.tif_job5852Indica Labs - HighPlex FL v4.1.3_object_results.csv.gz"
di=read_csv(hfile) %>% 
mutate(SID=gsub(".* S19","S19",`Image Location`)%>%gsub(" .*","",.)) %>% 
rename(Region=`Classifier Label`) %>% 
select(SID,Region,ObjectID=`Object Id`,matches(" Positive Classification$|^[XY]"))
di
markers=grep(" Positive Classification$",colnames(di),value=T) %>%
gsub(" Positive Classification","",.)
di=di %>%
rename_all(~gsub(" Positive.*","",.)) %>% 
mutate(x0=(XMax+XMin)/2,y0=(YMax+YMin)/2)
DI
di
di %>% gather(Marker,Pos,all_of(markers))
require(tidyr)
di %>% gather(Marker,Pos,all_of(markers))
di=read_csv(hfile) %>% 
mutate(SID=gsub(".* S19","S19",`Image Location`)%>%gsub(" .*","",.)) %>% 
rename(Region=`Classifier Label`) %>% 
select(SID,Region,ObjectID=`Object Id`,matches(" Positive Classification$|^[XY]"))
markers=grep(" Positive Classification$",colnames(di),value=T) %>%
gsub(" Positive Classification","",.)
di=di %>%
rename_all(~gsub(" Positive.*","",.)) %>% 
mutate(x0=(XMax+XMin)/2,y0=-(YMax+YMin)/2)
di
file.exists("meta/markerRename.csv")
if(file.exists("meta/markerRename.csv"))
markerRename=read_csv("meta/markerRename.csv")
markerRename
left_join(di,markerRename,by=c(Marker="Orig"))
di
di=read_csv(hfile) %>% 
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
markerRename=read_csv("meta/markerRename.csv")
}
markerRename
left_join(di,markerRename,by=c(Marker="Orig"))
left_join(di,markerRename,by=c(Marker="Orig")) %>% mutate(Marker=New) %>% select(-New)
left_join(di,markerRename,by=c(Marker="Orig")) %>% mutate(Marker=New) %>% select(-New) %>% count(Marker)
di
source("read_halo.R")
oo=read_halo(hfile)
di=oo$dat
di
di %>% count(Marker,Pos)
di %>% count(Marker,Pos) %>% spread(Marker,n)
di %>% count(Marker,Pos) %>% spread(Pos,n)
hfile
di
di %>% count(SID,Marker,Pos) %>% spread(Pos,n)
di
di %>% filter(Pos==10
di %>% filter(Pos==1)
di %>% filter(Pos==1)
di %>% filter(Pos==1) %>% select(ObjectID,Marker)
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID)
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker)
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% mutate(Marker=paste0(">",Marker,"<")
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% mutate(Marker=paste0(">",Marker,"<"))
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(Markers=list(Marker))
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(Markers=list(Marker),Nmark=len(Marker)) 
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(Markers=list(Marker),Nmark=len(Marker)) %>% arrange(desc(Nmark))
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(PosMarkers=list(Marker),PM=paste(Marker,collapse=";"))
pm=di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(PosMarkers=list(Marker),PM=paste(Marker,collapse=";"))

pm$PosMarkers[1]
pm %>% filter("CD20" %in% PM[[1]])
pm %>% filter("CD20" %in% .dat[[PM]][[1]])
pm %>% filter("CD20" %in% .[[PM]][[1]])
pm %>% rowwise %>% filter("CD20" %in% .[[PM]][[1]])
pm %>% rowwise %>% filter("CD20" %in% PM[[1]])
pm %>% rowwise %>% filter("CD20" %in% PM)
pm$PosMarkers[1]
ll=pm$PosMarkers[1]
"CD20" %in% ll
"CD20" %in% ll[[1]]
"CD20" %in% names(ll)
pm=di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID)
pm
pm %>% filter("CD20" %in% Marker)
left_join(di,markerRename,by=c(Marker="Orig")) %>% mutate(Marker=New) %>% select(-New) %>% count(Marker)
left_join(di,markerRename,by=c(Marker="Orig")) %>% mutate(Marker=New) %>% select(-New) %>% count(Marker,Pos)
left_join(di,markerRename,by=c(Marker="Orig")) %>% mutate(Marker=New) %>% select(-New) %>% count(Marker,Pos) %>% spread(Marker,Pos)
left_join(di,markerRename,by=c(Marker="Orig")) %>% mutate(Marker=New) %>% select(-New) %>% count(Marker,Pos) %>% spread(Marker,n)
hfile
di=read_csv(hfile) %>% 
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
markerRename=read_csv("meta/markerRename.csv")
di=left_join(di,markerRename,by=c(Marker="Orig")) %>%
mutate(Marker=New)
}
di
di %>% filter(is.na(New))
di %>% filter(is.na(Marker))
oo$dat %>% filter(is.na(New))
oo$dat
oo$dat %>% filter(is.na(Marker))
history()
history(1000)
di
oo$dat
oo$dat %>% count(Marker,Pos)
oo$dat %>% count(Marker,Pos) %>% data.frame
di=oo$dat
di
di %>% select(ObjectId,Marker)
di %>% filter(Pos==1) %>% select(ObjectID,Marker)
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker)
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% filter("CD163" %in% Marker)
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% filter("CD163" %in% Marker)
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% filter("CD163" %in% Marker)
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% mutate(Tag=case_when("CD3" %in% Marker))
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% mutate(Tag=case_when("CD3" %in% Marker ~ "t"))
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(PosMarkers=list(Marker))
di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(PosMarkers=list(Marker)) %>% distinct(PosMarkers)
pM=di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(PosMarkers=list(Marker)) %>% distinct(PosMarkers)
pM
pM=di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(PosMarkers=list(Marker),PMID=paste(Marker,collapse=","))
pM=di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(PosMarkers=list(Marker),PMID=paste(Marker,collapse=",")) %>%  distinct
pM
pM=di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(PosMarkers=list(Marker),PMID=paste(Marker,collapse=",")) %>%  distinct(PosMarkers,PM)
pM=di %>% filter(Pos==1) %>% select(ObjectID,Marker) %>% arrange(ObjectID,Marker) %>% group_by(ObjectID) %>% summarize(PosMarkers=list(Marker),PMID=paste(Marker,collapse=",")) %>%  distinct(PosMarkers,PMID)
pM
pM %>% mutate(Tag=case_when("CD30" %in% PosMarkers ~ "t"))
pM %>% mutate(Tag=case_when("CD30" %in% PosMarkers ~ "t")) %>% filter(!is.na(Tag))
pM %>% rowwise %>% mutate(Tag=case_when("CD30" %in% PosMarkers[[1]] ~ "t"))
pM %>% rowwise %>% mutate(Tag=case_when("CD30" %in% PosMarkers[[1]] ~ "t")) %>% filter(!is.na(Tag))
pM %>% rowwise %>% mutate(Tag=case_when("CD3" %in% PosMarkers[[1]] ~ "t")) %>% filter(!is.na(Tag))
pM
pM %>% unnest(PosMarkers)
pM %>% unnest(PosMarkers) %>% group_by(PMID)
pM %>% unnest(PosMarkers) %>% group_by(PMID) %>% mutate(Tag=case_when("CD3" %in% PosMarkers ~ "t"))
pM %>% unnest(PosMarkers) %>% group_by(PMID) %>% mutate(Tag=case_when("CD3" %in% PosMarkers ~ "t"))
pM$PosMarkers
pM$PosMarkers[[75]]
ll=pM$PosMarkers[[75]]
c("CD3","CD8") %in% ll
all(c("CD3","CD8") %in% ll)
all(c("CD3","CD11") %in% ll)
t
?t
t=c("CD3")
t
pM %>% unnest(PosMarkers) %>% group_by(PMID) %>% mutate(Tag=case_when(t %in% PosMarkers ~ "t"))
t_cd8=c("CD3","CD8")
pM %>% unnest(PosMarkers) %>% group_by(PMID) %>% mutate(Tag=case_when(t %in% PosMarkers ~ "t",all(t_cd8 %in% PosMarkers)~"t_cd8"))
pM %>% unnest(PosMarkers) %>% group_by(PMID) %>% mutate(Tag=case_when(t %in% PosMarkers ~ "t",all(t_cd8 %in% PosMarkers)~"t_cd8")) %>% count(Tag)
pM %>% unnest(PosMarkers) %>% group_by(PMID) %>% mutate(Tag=case_when(t %in% PosMarkers ~ "t",all(t_cd8 %in% PosMarkers)~"t_cd8")) %>% ungroup %>% count(Tag)
t_cd8
pM
apply(pM,\(x){t_cd8 %in% x})
sapply(pM,\(x){t_cd8 %in% x})
sapply(pM$PosMarkers,\(x){t_cd8 %in% x})
sapply(pM$PosMarkers,\(x){all(t_cd8 %in% x)})
pM[sapply(pM$PosMarkers,\(x){all(t_cd8 %in% x)}),]
readxl::read_xlsx("meta/proj_B-101-533__MetaData.xlsx")
pheno=readxl::read_xlsx("meta/proj_B-101-533__MetaData.xlsx")
pheno$Phenotypes[7]
strsplit(pheno$Phenotypes[7],"/"))
strsplit(pheno$Phenotypes[7],"/")
strsplit(pheno$Phenotypes[7],"/")[[1]]
grep("\\+$
mm=strsplit(pheno$Phenotypes[7],"/")[[1]]
grep("\\+$",mm)
grep("\\+$",mm,value=T)
grep("\\+$",mm,value=T) %>% gsub("\\+$","",.)
mPos=grep("\\+$",mm,value=T) %>% gsub("\\+$","",.)
mNeg=grep("-$",mm,value=T) %>% gsub("-$","",.)
mNeg
