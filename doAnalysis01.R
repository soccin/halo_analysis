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

#
# if there are >50 cells classified as CD3+/panCK+ in the
# Stroma classifier, please substract these from the analysis.

t_ie_Cells=cellTags %>% filter(Tag=="t_ie") %>% pull(C.UUID)

cellTagsFilt=cellTags %>%
    filter(!C.UUID %in% t_ie_Cells) %>%
    left_join(di %>% select(C.UUID,SID,Region),multiple="all") %>%
    mutate(Tag=factor(Tag))

# cellCountsFilt=cellTagsFilt %>%
#     left_join(di %>% select(C.UUID,SID,Region),multiple="all") %>%
#     mutate(Tag=factor(Tag)) %>%
#     group_by(SID,Region,Tag,.drop=F) %>%
#     count() %>%
#     arrange(n)

pg=list()
baseNTbl=list()

bases=fractions %>% distinct(Base) %>% pull

prows=c(1,1,2,2,2)
pcols=c(1,2,2,2,3)

for(base in bases) {

    targets=fractions %>% filter(Base==base) %>% distinct(Target) %>% pull(Target)
    regions=fractions %>% filter(Base==base) %>% distinct(Region) %>% pull(Region)

    baseCells=cellTagsFilt %>% filter(Phenotypes==base) %>% distinct(C.UUID) %>% pull
    baseTags=cellTagsFilt %>% filter(C.UUID %in% baseCells)

    bb=baseTags %>%
        left_join(distinct(phenoTable,Tag,Phenotypes)) %>%
        group_by(SID,Region,Tag) %>%
        count

    baseNTbl[[base]]=bb %>% spread(Tag,n)

    baseCounts=baseTags %>%
        left_join(distinct(phenoTable,Tag,Phenotypes)) %>%
        filter(Phenotypes==base) %>%
        group_by(SID,Region,Tag) %>%
        count %>%
        ungroup %>%
        select(-Tag) %>%
        rename(BaseN=n)

    pctBase=bb %>%
        ungroup %>%
        left_join(baseCounts) %>%
        mutate(PCT=n/BaseN) %>%
        select(-n,-BaseN) %>%
        left_join(distinct(phenoTable,Tag,Phenotypes)) %>%
        ungroup %>%
        filter(Phenotypes %in% targets & Region %in% regions)

    if(nrow(pctBase)>0) {
        nTargets=len(targets)
        pg[[len(pg)+1]]=pctBase %>%
            ggplot(aes(SID,PCT,fill=Region)) +
                geom_col(position=position_dodge(preserve="single"),color="grey35") +
                facet_wrap(~Tag,scales="free_y",ncol=pcols[nTargets],nrow=prows[nTargets]) +
                theme_minimal() +
                scale_fill_brewer(palette="Accent") +
                labs(title=paste("Base",base))
    } else {
        cat("\n No Cells in base",base,"targets",paste0(targets,collapse=","),"\n")
    }

}

pdf(file="proj_B-101-533_02.pdf",width=14,height=8.5)
print(pg)
dev.off()

cellTagsDistinct=cellTagsFilt %>%
    filter(!Tag %in% c("DAPI","t")) %>%
    group_by(C.UUID) %>%
    summarize(Tags=paste0(Tag,collapse=",")) %>%
    mutate(Tags=gsub(",.*","",Tags))

cells=di %>%
    select(C.UUID,SID,Region,x0,y0) %>%
    filter(C.UUID %in% cellTagsFilt$C.UUID) %>%
    left_join(cellTagsDistinct)

tagNames=cells %>% distinct(Tags) %>% filter(!is.na(Tags)) %>% arrange(Tags) %>% pull
tagColors=RColorBrewer::brewer.pal(len(tagNames),"Dark2")
names(tagColors)=tagNames

cells=cells %>% group_split(SID)

#library(ggrastr)

pg=list()
for(si in seq(cells)) {
    pg[[len(pg)+1]]=cells[[si]] %>% filter(Region!="Glass") %>% arrange(Tags) %>%
        ggplot(aes(x0,y0,color=Tags)) +
            geom_point(size=.4) +
            coord_fixed() +
            theme_minimal() +
            scale_color_manual(values=tagColors,na.value="grey75") +
            guides(colour = guide_legend(override.aes = list(size=5,alpha=1)))
}

pdf(file="proj_B-101-533_Cells.pdf",height=11,width=11)
print(pg)
dev.off()
