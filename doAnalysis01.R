args=commandArgs(trailing=T)

if(len(args)!=1) {
    cat("\n\tusage: getCounts.R phenoTypeFile\n\n")
    quit()
}

source("load_data.R")
require(ggplot2)

phenoFile=args[1]

##############################################
od=load_data(phenoFile)
di=od$di
phenoTable=od$phenoTable
##############################################

tbl1=di %>%
    select(C.UUID,PMID) %>%
    left_join(phenoTable) %>%
    group_by(PMID) %>%
    count(Tag) %>%
    spread(Tag,n,fill=0) %>%
    arrange(desc(DAPI))

openxlsx::write.xlsx(tbl1,"markerComboTableV3.xlsx")

fractions=readxl::read_xlsx(phenoFile,sheet="Fractions") %>% select(1:3)

#
# cellMarkerPos=di %>% select(C.UUID,PMID)
#

cellTags=di %>% select(C.UUID,PMID) %>% left_join(phenoTable,multiple="all")
cellCounts=left_join(cellTags,di %>% select(C.UUID,SID,Region),multiple="all") %>%
    mutate(Tag=factor(Tag)) %>%
    group_by(SID,Region,Tag,.drop=F) %>%
    count() %>%
    arrange(n)

source("plotTools.R")

halt("DDDD")

t_ie_Cells=cellTags %>% filter(Tag=="t_ie") %>% pull(C.UUID)

cellCountsFilt=cellTags %>% filter(!C.UUID %in% t_ie_Cells) %>% left_join(di %>% select(C.UUID,SID,Region),multiple="all") %>% mutate(Tag=factor(Tag)) %>% group_by(SID,Region,Tag,.drop=F) %>% count() %>% arrange(n)

pdf(file="cellCountsV3.pdf",width=14,height=8.5)
print(plotCounts(cellCounts)+ggtitle("All Cells"))
print(plotCounts(cellCountsFilt)+ggtitle("Removed CD3+/panCK+"))
dev.off()



dapi=cellCountsFilt %>% filter(Tag=="DAPI") %>% rename(DAPI=n) %>% ungroup %>% select(-Tag)

pct.dapi=cellCountsFilt %>% left_join(dapi) %>% mutate(PCT.DAPI=n/DAPI) %>% select(-n,-DAPI)

pg1=filter(pct.dapi,Tag!="DAPI") %>% ggplot(aes(SID,PCT.DAPI,fill=Region)) + geom_col(position=position_dodge(preserve="single"),color="grey35") + facet_wrap(~Tag,scales="free_y") + theme_minimal() + scale_fill_brewer(palette="Accent")

pdf(file="cellCountsV3.pdf",width=14,height=8.5)
print(plotCounts(cellCounts)+ggtitle("All Cells"))
print(plotCounts(cellCountsFilt)+ggtitle("Removed CD3+/panCK+"))
print(pg1)
dev.off()
