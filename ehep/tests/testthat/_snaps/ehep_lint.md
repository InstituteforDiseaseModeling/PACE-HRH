# Lint: Validation capture

    Code
      ehep::ValidateInputExcelFileContent(inputFile = "./bad_config/Test_validation.xlsx",
        outputDir = logdir, sheetNames = c("SeasonalityCurves"))
    Message <simpleMessage>
      Rule failed but No record-wise info:  Error in validate::violating(target, out[i]): Not all rules have record-wise output
      
    Error <simpleError>
      cannot coerce class '"function"' to a data.frame

