parse_safe=function (text)
{
    out <- vector("expression", length(text))
    for (i in seq_along(text)) {
        expr <- parse(text = text[[i]])
        out[[i]] <- if (length(expr) == 0) 
            NA
        else expr[[1]]
    }
    out
}

label_log1p=function (base = 10, digits = 3)
{
    function(x) {
        if (length(x) == 0) {
            return(expression())
        }
        exponent <- format(log(x, base = base), digits = digits)
        text <- paste0(base, "^", exponent)
        ret <- parse_safe(text)
        ret[is.na(x)] <- NA
        ret[x==0]=0
        ret
    }
}

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
