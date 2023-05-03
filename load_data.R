source("read_halo.R")
source("pheno_types.R")


load_data <- function(phenoFile) {

    oo=map(fs::dir_ls("raw",recur=T,regex="ObjectData.*.csv.gz$"),read_halo)

    if(length(oo)==0) {
        stop("\n\tCan not file Halo CSV files\n\tThey need to be in the subfolder '/raw'\n\n")
    }

    di=map(oo,"dat") %>% bind_rows

    phenoTable=get_pheno_table(di,phenoFile)

    cellMarkerPos=di %>%
        filter(Pos==1) %>%
        select(C.UUID,Marker) %>%
        group_by(C.UUID) %>%
        summarize(PMID=paste(Marker,collapse=",")) %>%
        ungroup

    di=di %>%
        left_join(cellMarkerPos,by = join_by(C.UUID)) %>%
        mutate(Marker=paste0("Marker:",Marker)) %>%
        spread(Marker,Pos) %>%
        left_join(phenoTable,by = join_by(PMID),multiple="all") %>%
        select(-Tag) %>%
        mutate(Phenotypes=paste0("Pheno:",Phenotypes)) %>%
        mutate(Pos=1) %>%
        spread(Phenotypes,Pos,fill=0)

    list(di=di,phenoTable=phenoTable)

}

# require(memoise)
# cdb <- cachem::cache_disk("./__RCache__")
# load_data<-memoise(.load_data,cache=cdb)
