# Sanity checks: scenario check

    Code
      pacehrh:::.checkScenarios(autoCorrect = FALSE)
    Output
      [1] FALSE

---

    Code
      pacehrh:::.checkScenarios(autoCorrect = TRUE)
    Message
      Loading scenarios sheet Scenarios
    Condition
      Warning:
      Columns with incorrect types in table: WeeksPerYr, HrsPerWeek
    Message
      Failed to load scenarios info from ./bad_config/model_inputs-bad_scenarios_sheet.xlsx
    Output
      [1] FALSE

---

    Code
      pacehrh:::.checkScenarios(autoCorrect = TRUE)
    Message
      Loading scenarios sheet Scenarios
    Output
      [1] TRUE

