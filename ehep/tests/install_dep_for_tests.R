packages = c('stringr', 'parallel', ' devtools', 'rcmdcheck')
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}

getDepPackages = function(descriptionpath) {
  fp = file(descriptionpath, "r")
  start = F
  done = F
  while ( !done ) {
    line = readLines(fp, n = 1)
    if (grepl("Imports:",line)) {
      start = T
      next
    }
    else if (start) {
      if (!grepl(":", line)) {
        dep_name = gsub(',', '', str_trim(line))
        print(paste("install", dep_name, "with", detectCores(), "cores", sep=" "))
        options(install.packages.compile.from.source = "always")
        cmd <- paste( "install.packages('", dep_name, "',", "Ncpus = ", detectCores(), ")", sep="")
        eval(parse(text=cmd))
      }
      else {
        start = F
        done = T
        break
      }
    }
  }
  close(fp)
}

getDepPackages("../DESCRIPTION")
