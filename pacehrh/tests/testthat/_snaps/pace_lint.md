# Lint: basic

    Code
      errCode <- pacehrh::CheckInputExcelFileFormat(noDate = TRUE)
    Output
      
      Input file = ../../config/model_inputs.xlsx
      Sheet references from scenarios ... OK
      No duplicate tasks in seasonality offsets table ... OK
      The following tasks in the scenario offsets table are not used in task values sheets ...
      - TV_Comprehensive : OK
      * TV_Basic : FH.MN.D.3
      * TV_Basic : FH.MN.D.5
      * TV_Basic : FH.MN.D.4
      * TV_Basic : FH.MN.PNC.8
      * TV_Merged : FH.MN.ANC.2
      * TV_Merged : FH.MN.D.3
      * TV_Merged : FH.MN.D.5
      * TV_Merged : FH.MN.D.4
      * TV_Merged : FH.MN.PNC.8
      * TV_Merged : FH.MN.21A
      * TV_Merged : FH.Ntr.68
      * TV_Merged : FH.MN.21
      * TV_Merged : FH.MN.22
      * TV_Merged : DPC.Mlr.103A
      * TV_Merged : DPC.Mlr.103B
      * TV_Merged : DPC.Mlr.103C
      * TV_Merged : DPC.Mlr.104A
      * TV_Merged : DPC.Mlr.104B
      The following tasks are duplicated in task values sheets ...
      - TV_Comprehensive : OK
      - TV_Basic : OK
      - TV_Merged : OK
      The following numeric columns contain non-numeric values ...
      - TV_Comprehensive : OK
      - TV_Basic : OK
      - TV_Merged : OK
      The following seasonality curves aren't normalized to 1 ...
      - TV_Comprehensive : OK
      Test results: 0-0-1-0-0-0

---

    Code
      errCode <- pacehrh::CheckInputExcelFileFormat(noDate = TRUE)
    Output
      
      Input file = ./bad_config/model_inputs-malformed.xlsx
      The following sheets referenced by scenarios do not exist ...
      * NotASheet
      * AlsoNotASheet
      The following tasks are duplicated in the seasonality offsets table ...
      * DPC.TB.1
      * DPC.M.2ChTP
      The following tasks in the scenario offsets table are not used in task values sheets ...
      * TaskValues : DPC.M.2
      * TaskValues : NotATaskID
      * TEST_TaskValues_1 : FH.MC.1
      * TEST_TaskValues_1 : FH.MC.2
      * TEST_TaskValues_1 : FH.MC.3
      * TEST_TaskValues_1 : FH.MC.4
      * TEST_TaskValues_1 : FH.MC.5
      * TEST_TaskValues_1 : FH.Im.1
      * TEST_TaskValues_1 : FH.Im.2
      * TEST_TaskValues_1 : FH.Im.3
      * TEST_TaskValues_1 : FH.Im.4
      * TEST_TaskValues_1 : FH.N.1
      * TEST_TaskValues_1 : FH.N.2
      * TEST_TaskValues_1 : FH.N.3
      * TEST_TaskValues_1 : FH.N.4
      * TEST_TaskValues_1 : DPC.M.1AdTP
      * TEST_TaskValues_1 : DPC.M.1ChTP
      * TEST_TaskValues_1 : DPC.M.1neg
      * TEST_TaskValues_1 : DPC.M.2AdTP
      * TEST_TaskValues_1 : DPC.M.2ChTP
      * TEST_TaskValues_1 : DPC.M.2
      * TEST_TaskValues_1 : DPC.TB.1
      * TEST_TaskValues_1 : DPC.TB.2
      * TEST_TaskValues_1 : DPC.TB.3
      * TEST_TaskValues_1 : NotATaskID
      The following tasks are duplicated in task values sheets ...
      * TaskValues : FH.MC.1
      * TaskValues : FH.MC.2
      * TaskValues : FH.MC.3
      * TaskValues : FH.MC.4
      * TaskValues : FH.MC.5
      * TaskValues : FH.MC.6
      * TaskValues : FH.MC.7
      * TaskValues : FH.MC.8
      * TaskValues : FH.Im.1
      * TaskValues : FH.Im.2
      * TaskValues : FH.Im.3
      * TaskValues : FH.Im.4
      * TaskValues : FH.Im.5
      * TaskValues : FH.N.1
      * TaskValues : FH.N.2
      * TaskValues : FH.N.3
      * TaskValues : FH.N.4
      * TaskValues : Travel
      * TaskValues : Administration
      * TaskValues : Record keeping
      * TaskValues : MHH
      * TaskValues : DPC.TB.1
      * TaskValues : DPC.TB.2
      * TaskValues : DPC.TB.3
      * TaskValues : DPC.M.1AdTP
      * TaskValues : DPC.M.2AdTP
      * TaskValues : DPC.M.1ChTP
      * TaskValues : DPC.M.2ChTP
      * TaskValues : DPC.M.1neg
      * TaskValues : DPC.M.3
      * TaskValues : DPC.FA.1
      * TaskValues : DPC.H.1
      * TaskValues : DPC.H.2
      * TaskValues : DPC.CVD.1
      * TaskValues : DPC.D.1
      * TaskValues : DPC.D.2
      * TaskValues : DPC.MH.2
      * TaskValues : DPC.MH.3
      * TaskValues : DPC.NTD.3
      * TaskValues : DPC.HIV.1
      * TaskValues : DPC.HIV.3
      * TaskValues : DPC.HIV.2
      * TaskValues : FH.FP.1A
      * TaskValues : FH.FP.1B
      * TaskValues : FH.FP.1C
      * TaskValues : FH.FP.1D
      * TaskValues : FH.FP.2
      * TaskValues : FH.FP.3
      * TaskValues : FH.FP.4
      * TaskValues : FH.FP.5
      * TaskValues : FH.FP.6
      - TEST_TaskValues_1 : OK
      The following numeric columns contain non-numeric values ...
      - TaskValues : OK
      * TEST_TaskValues_1 : StartingRateInPop
      * TEST_TaskValues_1 : RateMultiplier
      The following seasonality curves aren't normalized to 1 ...
      * TaskValues : Malnutrition (12)
      * TaskValues : TB (11.948)
      * TEST_TaskValues_1 : Malnutrition (12)
      * TEST_TaskValues_1 : TB (11.948)
      Test results: 1-1-1-1-1-1

# Lint: bad scenario sheet name

    Code
      errCode <- pacehrh::CheckInputExcelFileFormat(scenarioSheet = "notasheet",
        noDate = TRUE)
    Output
      
      Input file = ../../config/model_inputs.xlsx
      notasheet sheet could not be found

# Lint: bad seasonality offsets sheet name

    Code
      errCode <- pacehrh::CheckInputExcelFileFormat(seasonalityOffsetsSheet = "notasheet",
        noDate = TRUE)
    Output
      
      Input file = ../../config/model_inputs.xlsx
      notasheet sheet could not be found

# Lint: non-existent input file

    Code
      errCode <- pacehrh::CheckInputExcelFileFormat(inputFile = "notafile", noDate = TRUE)
    Output
      
      Input file = notafile
      notafile not found

---

    Code
      errCode <- pacehrh::CheckInputExcelFileFormat(inputFile = "globalconfig.json",
        noDate = TRUE)
    Output
      
      Input file = globalconfig.json
      globalconfig.json could not be read
      Error message: Error: Can't establish that the input is either xls or xlsx.
      

