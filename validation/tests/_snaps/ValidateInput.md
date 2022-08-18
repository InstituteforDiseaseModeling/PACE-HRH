# Validation capture

    Code
      ValidateInputExcelFileContent(inputFile = "validation/tests/sample_config/Test_validation.xlsx",
        outputDir = logdir, sheetNames = c("SeasonalityCurves"))
    Message <simpleMessage>
      Rule failed but No record-wise info:  Error in validate::violating(target, out[i]): Not all rules have record-wise output
      
    Message <rlang_message>
      Joining, by = "name"
    Output
      [1] -11

