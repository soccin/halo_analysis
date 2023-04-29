args=commandArgs(trailing=T)

if(len(args)!=1) {
    cat("\n\tusage: getCounts.R phenoTypeFile\n\n")
    quit()
}

source("load_data.R")
source("plotTools.R")

require(ggplot2)

phenoFile=args[1]

##############################################
od=load_data(phenoFile)
di=od$di
phenoTable=od$phenoTable
##############################################

fractions=readxl::read_xlsx(phenoFile,sheet="Fractions") %>% select(1:3)

#
# cellMarkerPos=di %>% select(C.UUID,PMID)
#

cellTags=di %>% select(C.UUID,PMID) %>% left_join(phenoTable,multiple="all")

t_ie_Cells=cellTags %>% filter(Tag=="t_ie") %>% pull(C.UUID)

cellCountsFilt=cellTags %>%
    filter(!C.UUID %in% t_ie_Cells) %>%
    left_join(di %>% select(C.UUID,SID,Region),multiple="all") %>%
    mutate(Tag=factor(Tag)) %>%
    group_by(SID,Region,Tag,.drop=F) %>%
    count() %>%
    arrange(n)

dapi=cellCountsFilt %>% filter(Tag=="DAPI") %>% rename(DAPI=n) %>% ungroup %>% select(-Tag)

pct.dapi=cellCountsFilt %>% left_join(dapi) %>% mutate(PCT.DAPI=n/DAPI) %>% select(-n,-DAPI)

pg1=filter(pct.dapi,Tag!="DAPI") %>% ggplot(aes(SID,PCT.DAPI,fill=Region)) + geom_col(position=position_dodge(preserve="single"),color="grey35") + facet_wrap(~Tag,scales="free_y") + theme_minimal() + scale_fill_brewer(palette="Accent")

pdf(file="proj_B-101-533_01.pdf",width=14,height=8.5)
print(pg1)
dev.off()
