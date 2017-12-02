tidyTitle = function(title){

  if(any(is.na(title))){
    title[is.na(title)] = ""
  }

  proper = function(x){
    gsub("(?<=\\b)([[:alpha:]])", "\\U\\1", x, perl = TRUE)
  }

  correctCapitals = function(x){
    allowed = "(PLS|LSMM|ALTREP|NZ|GARCH|DNA|II|EWMA|R&amp;D|P\\(X&lt;Y\\)|gridSVG|von[-]?Mises|ARMA|SPSS|SVM|BIG-SIR|SSREM|GWAS|IGESS|LSSM|BIVAS)"
    x = gsub(allowed, "\\U\\1", x, ignore.case = TRUE, perl = TRUE)
    x = gsub("GRIDSVG", "gridSVG", x)
    x = gsub("VON-MISES", "von Mises", x)
    x = gsub("&AMP", "&amp", x)
    x = gsub("&lt", "&lt", x)
    x = gsub("[’']S", "'s", x)
    x = gsub("&Hellip;", "&hellip;", x)

    return(x)
  }

  title = title %>%
    proper() %>%
    correctCapitals()

  return(title)
}
