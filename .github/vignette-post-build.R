# The code in this script inserts Google Tag Manager tracking code into vignette HTML files.

firstTag <- c(
  "<!-- Google Tag Manager -->",
  "<script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':",
  "new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],",
  "j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=",
  "'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);",
  "})(window,document,'script','dataLayer','GTM-NK4K647');</script>",
  "<!-- End Google Tag Manager -->"
)

secondTag <- c(
  "<!-- Google Tag Manager (noscript) -->",
  "<noscript><iframe src='https://www.googletagmanager.com/ns.html?id=GTM-NK4K647'",
  "height='0' width='0' style='display:none;visibility:hidden'></iframe></noscript>",
  "<!-- End Google Tag Manager (noscript) -->"
)

processLines <- function(inConn, outConn) {
  inHead <- FALSE
  inBody <- FALSE

  while (TRUE) {
    line <- readLines(inConn, n = 1)

    if (length(line) == 0) {
      break
    }

    writeLines(line, outConn)

    if (!inHead && !inBody) {
      if (grepl("<head>", line, ignore.case = TRUE)) {
        inHead <- TRUE
        for (l in firstTag){
          writeLines(l, outConn)
        }
      }
      else if (grepl("<body>", line, ignore.case = TRUE)) {
        inBody <- TRUE
        for (l in secondTag){
          writeLines(l, outConn)
        }
      }
    } else if (inHead) {
      if (grepl("<\\/head>", line, ignore.case = TRUE)) {
        inHead <- FALSE
      }
    } else if (inBody) {
      if (grepl("<\\/body>", line, ignore.case = TRUE)) {
        inBody <- FALSE
      }
    }
  }
}

processFile <- function(inFile, outFile) {
  if (file.exists(outFile)) {
    print(paste0("Over-writing output file ", outFile))
    file.remove(outFile)
  }

  returnValue <- TRUE

  inConn <- NULL
  outConn <- NULL

  tryCatch({
    inConn <- file(inFile, "rt")
    outConn <- file(outFile, "wt")

    processLines(inConn, outConn)
  },
  error = function(e) {
    print("processFile failed")
    print(e)
    returnValue <<- FALSE
  },
  finally = {
    if (!is.null(outConn)) {
      close(outConn)
    }

    if (!is.null(inConn)) {
      close(inConn)
    }
  })

  return(returnValue)
}

processFiles <- function(files) {
  outFile <- file.path(getwd(), "vignettes", "out.tmp")

  for (file in files) {
    print(paste0("Processing ", file))
    inFile <- file.path(getwd(), "vignettes", file)
    result <- processFile(inFile, outFile)

    if (result) {
      file.remove(inFile)
      file.rename(outFile, inFile)
    }
  }
}

vignettes <- c("pacehrh.html", "technical-notes.html", "results-struct.html", "input-validation.html", "graphics.html")
processFiles(vignettes)
