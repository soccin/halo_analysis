source("read_halo.R")
source("pheno_types.R")

oo=map(fs::dir_ls("raw",recur=T,regex="ObjectData.*.csv.gz$"),read_halo)
di=map(oo,"dat") %>% bind_rows

phenoTable=get_pheno_table(di)

tbl=di %>%
    filter(Pos==1) %>%
    select(SID,ObjectID,Marker) %>%
    arrange(SID,ObjectID,Marker) %>%
    group_by(SID,ObjectID) %>%
    summarize(PMID=paste(Marker,collapse=",")) %>%
    ungroup %>%
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

