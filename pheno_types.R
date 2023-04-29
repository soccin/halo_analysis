suppressMessages(require(purrr))

get_pos_markers<-function(di) {

    di %>%
        filter(Pos==1) %>%
        select(C.UUID,Marker) %>%
        group_by(C.UUID) %>%
        summarize(PosMarkers=list(Marker),PMID=paste(Marker,collapse=",")) %>%
        distinct(PosMarkers,PMID) %>%
        mutate(Tag=list("all")) %>%
        ungroup %>%
        distinct

}

get_pheno_table<-function(di,pfile) {

    pMt=get_pos_markers(di) %>% transpose

    pheno=readxl::read_xlsx(pfile)
    pp=transpose(pheno)

    for(ii in seq(pp)) {
        pi=pp[[ii]]
        mm=strsplit(pi$Phenotypes,"/")[[1]]
        mPos=grep("\\+$",mm,value=T) %>% gsub("\\+$","",.)
        mNeg=grep("-$",mm,value=T) %>% gsub("-$","",.)

        for(jj in seq(pMt)) {
            pMi=pMt[[jj]]
            if(setequal(intersect(pMi$PosMarkers,mPos),mPos) && len(intersect(mNeg,pMi$PosMarkers))==0) {
                print(pi$Tag)
                print(pMi)
                pMi$Tag=append(pMi$Tag,pi$Tag)
                pMt[[jj]]=pMi
            }
        }
    }

    map(pMt,\(x){x$PosMarkers=NULL;x}) %>%
        map(as_tibble) %>%
        bind_rows %>%
        filter(Tag!="all")

}

