args=commandArgs(trailing=T)

if(len(args)!=1) {
    cat("\n\tusage: getCounts.R phenoTypeFile\n\n")
    quit()
}

require(ggplot2)

phenoFile=args[1]

source("read_halo.R")
source("pheno_types.R")

oo=map(fs::dir_ls("raw",recur=T,regex="ObjectData.*.csv.gz$"),read_halo)
di=map(oo,"dat") %>% bind_rows

phenoTable=get_pheno_table(di,phenoFile)
phenoTableCollapse=phenoTable %>%
    group_by(PMID) %>%
    summarize(Tags=paste0(Tag,collapse=",")) %>%
    ungroup

cellMarkerPos=di %>%
    filter(Pos==1) %>%
    select(C.UUID,Marker) %>%
    group_by(C.UUID) %>%
    summarize(PMID=paste(Marker,collapse=",")) %>%
    ungroup

tbl=cellMarkerPos %>%
    count(PMID) %>%
    arrange(desc(n)) %>%
    ungroup %>%
    left_join(phenoTable,multiple="all") %>%
    mutate(Tag=ifelse(is.na(Tag),".NULL",Tag))

totals=tbl %>%
    group_by(PMID) %>%
    summarize(Total=min(n)) %>%
    arrange(desc(Total))

tbl1=tbl %>%
    spread(Tag,n) %>%
    left_join(totals) %>%
    arrange(desc(Total)) %>%
    select(-Total)

openxlsx::write.xlsx(tbl1,"markerComboTableV2.xlsx")

fractions=readxl::read_xlsx(phenoFile,sheet="Fractions") %>% select(1:3)

cellTags=cellMarkerPos %>% left_join(phenoTable,multiple="all")
cellCounts=left_join(cellTags,di %>% select(C.UUID,SID,Region),multiple="all") %>% mutate(Tag=factor(Tag)) %>% group_by(SID,Region,Tag,.drop=F) %>% count() %>% arrange(n)

source("plotTools.R")

plotCounts<-function(cellCounts) {
    cellCounts %>% ggplot(aes(Tag,n,fill=Region)) +
        geom_col(position=position_dodge(preserve="single"),color="grey35") +
        facet_wrap(~SID,scales='free_y') +
        coord_flip() +
        scale_y_continuous(trans="log1p",breaks=c(0,10^(1:6)),labels=label_log1p()) +
        geom_hline(yintercept=0) +
        scale_fill_brewer(palette="Accent") +
        theme_minimal()
}


t_ie_Cells=cellTags %>% filter(Tag=="t_ie") %>% pull(C.UUID)

cellCountsFilt=cellTags %>% filter(!C.UUID %in% t_ie_Cells) %>% left_join(di %>% select(C.UUID,SID,Region),multiple="all") %>% mutate(Tag=factor(Tag)) %>% group_by(SID,Region,Tag,.drop=F) %>% count() %>% arrange(n)

pdf(file="cellCountsV1.pdf",width=14,height=8.5)
print(plotCounts(cellCounts)+ggtitle("All Cells"))
print(plotCounts(cellCountsFilt)+ggtitle("Removed CD3+/panCK+"))
dev.off()

dapi=cellCountsFilt %>% filter(Tag=="DAPI") %>% rename(DAPI=n) %>% ungroup %>% select(-Tag)

pct.dapi=cellCountsFilt %>% left_join(dapi) %>% mutate(PCT.DAPI=n/DAPI) %>% select(-n,-DAPI)

pg1=filter(pct.dapi,Tag!="DAPI") %>% ggplot(aes(SID,PCT.DAPI,fill=Region)) + geom_col(position=position_dodge(preserve="single"),color="grey35") + facet_wrap(~Tag,scales="free_y") + theme_minimal() + scale_fill_brewer(palette="Accent")

pdf(file="cellCountsV1.pdf",width=14,height=8.5)
print(plotCounts(cellCounts)+ggtitle("All Cells"))
print(plotCounts(cellCountsFilt)+ggtitle("Removed CD3+/panCK+"))
print(pg1)
dev.off()
