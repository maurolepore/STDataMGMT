# the strucutre of the datasets remains the same

    Code
      names(scenarios_geographies)
    Output
      [1] "scenario_geography"         "country_iso"               
      [3] "reg_count"                  "scenario_geography_newname"
      [5] "country"                   

---

    Code
      lapply(scenarios_geographies, typeof)
    Output
      $scenario_geography
      [1] "character"
      
      $country_iso
      [1] "character"
      
      $reg_count
      [1] "integer"
      
      $scenario_geography_newname
      [1] "character"
      
      $country
      [1] "character"
      

---

    Code
      names(production_types)
    Output
      [1] "ald_sector"            "ald_business_unit"     "ald_production_unit"  
      [4] "emissions_factor_unit"

---

    Code
      lapply(production_types, typeof)
    Output
      $ald_sector
      [1] "character"
      
      $ald_business_unit
      [1] "character"
      
      $ald_production_unit
      [1] "character"
      
      $emissions_factor_unit
      [1] "character"
      

---

    Code
      names(synthetic_company_activities)
    Output
       [1] "company_id"            "company_name"          "ald_sector"           
       [4] "ald_business_unit"     "activity_unit"         "ald_location"         
       [7] "Equity Ownership 2021" "Equity Ownership 2022" "Equity Ownership 2023"
      [10] "Equity Ownership 2024" "Equity Ownership 2025"

---

    Code
      lapply(synthetic_company_activities, typeof)
    Output
      $company_id
      [1] "integer"
      
      $company_name
      [1] "character"
      
      $ald_sector
      [1] "character"
      
      $ald_business_unit
      [1] "character"
      
      $activity_unit
      [1] "character"
      
      $ald_location
      [1] "character"
      
      $`Equity Ownership 2021`
      [1] "double"
      
      $`Equity Ownership 2022`
      [1] "double"
      
      $`Equity Ownership 2023`
      [1] "double"
      
      $`Equity Ownership 2024`
      [1] "double"
      
      $`Equity Ownership 2025`
      [1] "double"
      

---

    Code
      names(synthetic_company_emissions)
    Output
       [1] "company_id"            "company_name"          "ald_sector"           
       [4] "ald_business_unit"     "ald_location"          "activity_unit"        
       [7] "Equity Ownership 2021" "Equity Ownership 2022" "Equity Ownership 2023"
      [10] "Equity Ownership 2024" "Equity Ownership 2025"

---

    Code
      lapply(synthetic_company_emissions, typeof)
    Output
      $company_id
      [1] "integer"
      
      $company_name
      [1] "character"
      
      $ald_sector
      [1] "character"
      
      $ald_business_unit
      [1] "character"
      
      $ald_location
      [1] "character"
      
      $activity_unit
      [1] "character"
      
      $`Equity Ownership 2021`
      [1] "integer"
      
      $`Equity Ownership 2022`
      [1] "integer"
      
      $`Equity Ownership 2023`
      [1] "integer"
      
      $`Equity Ownership 2024`
      [1] "integer"
      
      $`Equity Ownership 2025`
      [1] "integer"
      

---

    Code
      names(synthetic_eikon_data)
    Output
      [1] "company_id"        "isin"              "pd"               
      [4] "net_profit_margin" "debt_equity_ratio" "volatility"       
      [7] "asset_drift"       "ald_location"     

---

    Code
      lapply(synthetic_eikon_data, typeof)
    Output
      $company_id
      [1] "integer"
      
      $isin
      [1] "character"
      
      $pd
      [1] "double"
      
      $net_profit_margin
      [1] "double"
      
      $debt_equity_ratio
      [1] "double"
      
      $volatility
      [1] "double"
      
      $asset_drift
      [1] "double"
      
      $ald_location
      [1] "character"
      

