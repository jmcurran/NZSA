tidyTitle = function(title){

  if(is.na(title)){
    return("")
  }

  proper = function(x){
    gsub("(?<=\\b)([a-z])", "\\U\\1", x, perl = TRUE)
  }

  correctCapitals = function(x){
    allowed = "(NZ|GARCH|DNA|II|EWMA|R&amp;D|P\\(X&lt;Y\\)|gridSVG|von[-]?Mises|ARMA|SPSS|SVM|BIG-SIR|SSREM|GWAS|IGESS|LSSM|BIVAS)"
    x = gsub(allowed, "\\U\\1", x, ignore.case = TRUE, perl = TRUE)
    x = gsub("GRIDSVG", "gridSVG", x)
    x = gsub("VON-MISES", "von Mises", x)
    x = gsub("&AMP", "&amp", x)
    x = gsub("&lt", "&lt", x)
    x = gsub("[’']S", "'s", x)

    return(x)
  }

  title = title %>%
    proper() %>%
    correctCapitals()

  return(title)
}
