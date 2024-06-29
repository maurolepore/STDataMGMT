
<!-- README.md is generated from README.Rmd. Please edit that file -->

# STDataMGMT

<!-- badges: start -->
<!-- badges: end -->

The goal of STDataMGMT is to generate private data for the stress test.

This repository is both an R package and a data-processing pipeline:

- To learn about the package see its website.
- To learn about the data-processing pipeline see data-raw/

``` r
list.files("data-raw")
#>  [1] "bench_regions.csv"                                   
#>  [2] "bench_regions.rds"                                   
#>  [3] "capacity_factors_data"                               
#>  [4] "countries_samplings"                                 
#>  [5] "LifeLogo2.jpg"                                       
#>  [6] "matchingregions.xlsx"                                
#>  [7] "prep_datalake.R"                                     
#>  [8] "price_data_long_data"                                
#>  [9] "raw_ngfs_carbon_price.csv"                           
#> [10] "run_prepare_abcd_stress_test_input.R"                
#> [11] "run_prepare_ngfs_carbon_price.R"                     
#> [12] "run_prepare_prewrangled_capacity_factors.R"          
#> [13] "run_prepare_prewrangled_financial_data_stress_test.R"
#> [14] "run_prepare_price_data_long.R"                       
#> [15] "run_prepare_Scenarios_AnalysisInput.R"               
#> [16] "run_rename_geographies.R"                            
#> [17] "run_workflow.R"                                      
#> [18] "sampling_scripts"                                    
#> [19] "scenario_analysis_input_data"                        
#> [20] "st_inputs"
```

## Installation

You can install the development version of STDataMGMT from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Theia-Finance-Labs/STDataMGMT")
```

## Example

``` r
library(STDataMGMT)
library(tibble)

# Some of the included dataset
production_types
#> # A tibble: 27 × 4
#>    ald_sector ald_business_unit   ald_production_unit emissions_factor_unit
#>    <chr>      <chr>               <chr>               <chr>                
#>  1 Power      RenewablesCap       MW                  tCO2e                
#>  2 Power      RenewablesCap       MWh                 tCO2e                
#>  3 Shipping   Freight             dwt km              tCO2                 
#>  4 Power      GasCap              MW                  tCO2e                
#>  5 Power      GasCap              MWh                 tCO2e                
#>  6 Power      OilCap              MW                  tCO2e                
#>  7 Power      OilCap              MWh                 tCO2e                
#>  8 Cement     Integrated facility t cement            tCO2e                
#>  9 Shipping   Passenger           dwt km              tCO2                 
#> 10 Oil&Gas    Gas                 GJ                  tCO2e                
#> # ℹ 17 more rows
```

``` r

scenarios_geographies
#> # A tibble: 2,509 × 5
#>    scenario_geography country_iso reg_count scenario_geography_newname country  
#>    <chr>              <chr>           <int> <chr>                      <chr>    
#>  1 Africa             DZ                 55 Africa                     Algeria  
#>  2 Africa             AO                 55 Africa                     Angola   
#>  3 Africa             BJ                 55 Africa                     Benin    
#>  4 Africa             BW                 55 Africa                     Botswana 
#>  5 Africa             BF                 55 Africa                     Burkina …
#>  6 Africa             BI                 55 Africa                     Burundi  
#>  7 Africa             CV                 55 Africa                     Cape Ver…
#>  8 Africa             CM                 55 Africa                     Cameroon 
#>  9 Africa             CF                 55 Africa                     Central …
#> 10 Africa             TD                 55 Africa                     Chad     
#> # ℹ 2,499 more rows
```

``` r

synthetic_eikon_data
#> # A tibble: 604 × 8
#>    company_id isin            pd net_profit_margin debt_equity_ratio volatility
#>         <int> <chr>        <dbl>             <dbl>             <dbl>      <dbl>
#>  1         71 GE6628588125 0.284            0.226              0.950      0.334
#>  2         36 HT3395729239 0.255            0.285              0.282      0.319
#>  3         14 AE9837146345 0.407            0.681              0.624      0.314
#>  4         89 AM9162167624 0.435            0.346              0.242      0.802
#>  5         99 TZ1549767986 0.594            0.914              0.344      0.639
#>  6         87 KI8336843677 0.394            0.761              0.824      0.191
#>  7         24 BS9152773656 0.805            0.234              0.332      0.906
#>  8         87 GN5796334323 0.150            0.0492             0.437      0.199
#>  9         18 SY5491274579 0.558            0.468              0.937      0.168
#> 10         71 CG1135897874 0.404            0.887              0.109      0.544
#> # ℹ 594 more rows
#> # ℹ 2 more variables: asset_drift <dbl>, ald_location <chr>
```

## Funding

The repository forms part of the LIFE STRESS project. The LIFE STRESS
project has received funding from the LIFE Programme of the European
Union. The contents of this publication are the sole responsibility of
Theia Finance Labs and do not necessarily reflect the opinion of the
European Union. Project: 101074271 — LIFE21-GIC-DE-Stress.

EU LIFE Project Grant

Co-funded by the European Union. Views and opinions expressed are
however those of the author(s) only and do not necessarily reflect those
of the European Union or CINEA. Neither the European Union nor the
granting authority can be held responsible for them.

Scientific Transition Risk Exercises for Stress tests & Scenario
Analysis has received funding from the European Union’s Life programme
under Grant No. LIFE21-GIC-DE-Stress under the LIFE-2021-SAP-CLIMA
funding call.

<img src=data-raw/LifeLogo2.jpg width=150>
