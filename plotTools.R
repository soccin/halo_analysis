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

