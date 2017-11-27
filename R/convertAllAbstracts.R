convertAllAbstracts = function(inputPath = "~/Dropbox/Work/2017/Conferences/NZSA-IASC/NZSA-IASC-Prog/Submissions/",
                               outputPath = "~/Dropbox/Work/2017/Conferences/NZSA-IASC/NZSA-IASC-Prog/Submissions/Rmd/"){

  Files = list.files(path = inputPath, pattern = "txt|tex")

  for(f1 in Files){
    inFile = paste0(inputPath, f1)
    outFile = gsub("(^.*)\\.(tex|txt)$", "\\1.Rmd", paste0(outputPath, f1))
    cat(paste(f1, "\n"))
    pandoc_convert(input = inFile, from = "latex", to = "markdown", output = outFile)

    Lines = readLines(outFile)
    if(!any(grepl("<span>[*]{2}Abstract[*]{2}</span>", Lines))){
      if(any(grepl("abstract", Lines, ignore.case = TRUE))){
        i = grep("abstract", Lines, ignore.case = TRUE)
        Lines[i] = gsub("abstract", "<span>**Abstract**</span>", Lines[i])
      }else{
        Lines = c("<span>**Abstract**</span>", Lines)
      }
      cat(paste0("Ammending ", f1, "\n"))
      writeLines(Lines, outFile)
    }
  }
}
