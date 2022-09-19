options(install.packages.compile.from.source = "always")
packages = c('stringr', 'parallel')
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}

getDepPackages = function(descriptionpath, section='Imports:') {
  fp = file(descriptionpath, "r")
  start = F
  done = F
  while ( !done ) {
    line = readLines(fp, n = 1)
    if (grepl(section,line)) {
      start = T
      next
    }
    else if (start) {
      if (!grepl(":", line)) {
        dep_name = gsub(',', '', str_trim(line))
        print(paste("install", dep_name, "with", detectCores(), "cores", sep=" "))
        cmd <- paste( "install.packages('", dep_name, "',", "Ncpus = ", detectCores(), ")", sep="")
        eval(parse(text=cmd))
      }
      else {
        start = F
        done = T
        install.packages("rcmdcheck", Ncpus = detectCores())
        break
      }
    }
  }
  close(fp)
}

getDepPackages("../../pacehrh/DESCRIPTION", section='Imports:')
getDepPackages("../../pacehrh/DESCRIPTION", section='Suggests:')
