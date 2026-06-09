# Get raw data filtered by output type

Returns the raw ValueTB data filtered to a specified output type.

## Usage

``` r
get_data(cost_type = "ECON", output_name = NULL, output_group = NULL)
```

## Arguments

- cost_type:

  One of "ECON" or "FIN". If "ECON", model for economic costs is
  returned. If "FIN", model for financial costs is returned. Default
  "ECON".

- output_name:

  Optional character string specifying an output type to filter by.
  Defaults to NULL (all data). See [`outputs()`](outputs.md) for valid
  options.

- output_group:

  Optional character string specifying an output group to filter by.
  Defaults to NULL (all data). See [`output_groups()`](output_groups.md)
  for valid options.

## Value

A data frame containing only rows matching the specified output type.

## See also

outputs, output_groups

## Examples

``` r
head(get_data(output_group = "IP"))
#>   [1] fc_country                    fc_code                      
#>   [3] fc_geog1                      fc_type                      
#>   [5] fc_ownership                  fc_size_m2                   
#>   [7] fc_opvisits_total             fc_opvisits_TB               
#>   [9] fc_ipdays_total               fc_ipdays_tb                 
#>  [11] ss_lab_total                  ss_lab_tb                    
#>  [13] ss_rad_total                  ss_rad_tb                    
#>  [15] capacity                      fc_catchmentpop              
#>  [17] fc_timespent_TB               fc_FTE_facility_clin         
#>  [19] fc_FTE_facility_support       fc_FTE_total                 
#>  [21] fc_m2_TB                      fc_beds_facility             
#>  [23] fc_beds_TB                    fc_FTE_TB_doctors            
#>  [25] fc_FTE_TB_nurses              fc_FTE_TB_pharm              
#>  [27] fc_FTE_TB_lab                 fc_FTE_TB_rad                
#>  [29] fc_FTE_TB_odirect             fc_FTE_TB_admin              
#>  [31] fc_FTE_TB_support             fc_FTE_TB_vol                
#>  [33] fc_FTE_TB                     outputgroup                  
#>  [35] output                        originaloutputdescription    
#>  [37] servicestatistics             met_stafftime_obs            
#>  [39] met_stafftime_int             met_stafftime_tmst           
#>  [41] met_stafftime_ass             met_equip_obs                
#>  [43] met_equip_int                 met_supp_obs                 
#>  [45] met_supp_int                  met_TDvBU                    
#>  [47] met_FINvECON                  q_bldgspace                  
#>  [49] q_stafftime_clin              q_stafftime_nurs             
#>  [51] q_stafftime_pharm             q_stafftime_lab              
#>  [53] q_stafftime_rad               q_stafftime_odirect          
#>  [55] q_stafftime_admin             q_stafftime_support          
#>  [57] q_stafftime_vol               LCU_p_bldgspace              
#>  [59] LCU_p_stafftime_clin          LCU_p_stafftime_nurs         
#>  [61] LCU_p_stafftime_pharm         LCU_p_stafftime_lab          
#>  [63] LCU_p_stafftime_rad           LCU_p_stafftime_odirect      
#>  [65] LCU_p_stafftime_admin         LCU_p_stafftime_support      
#>  [67] LCU_p_stafftime_vol           LCU_cost_total_ohd_bldg      
#>  [69] LCU_cost_total_ohd_staff      LCU_cost_total_ohd_transport 
#>  [71] LCU_cost_total_ohd_other      LCU_cost_total_bldgs         
#>  [73] LCU_cost_total_equip_med      LCU_cost_total_equip_other   
#>  [75] LCU_cost_total_furniture      LCU_cost_total_vehicles      
#>  [77] LCU_cost_total_training       LCU_cost_total_staff_clin    
#>  [79] LCU_cost_total_staff_support  LCU_cost_total_staff_vol     
#>  [81] LCU_cost_total_supplies_med   LCU_cost_total_drugs         
#>  [83] LCU_cost_total_supplies_other LCU_cost_total_maintenance   
#>  [85] LCU_cost_total_utilities      LCU_cost_total_transport     
#>  [87] LCU_cost_total_food           LCU_cost_total_other         
#>  [89] LCU_unitcost_ohd_bldg         LCU_unitcost_ohd_staff       
#>  [91] LCU_unitcost_ohd_transport    LCU_unitcost_ohd_other       
#>  [93] LCU_unitcost_bldgs            LCU_unitcost_equip_med       
#>  [95] LCU_unitcost_equip_other      LCU_unitcost_furniture       
#>  [97] LCU_unitcost_vehicles         LCU_unitcost_training        
#>  [99] LCU_unitcost_staff_clin       LCU_unitcost_staff_support   
#> [101] LCU_unitcost_staff_vol        LCU_unitcost_supplies_med    
#> [103] LCU_unitcost_drugs            LCU_unitcost_supplies_other  
#> [105] LCU_unitcost_maintenance      LCU_unitcost_utilities       
#> [107] LCU_unitcost_transport        LCU_unitcost_food            
#> [109] LCU_unitcost_other            LCU_unitcost_total           
#> [111] LCU_unitcost_fixed            LCU_unitcost_variable        
#> [113] met_currency                  met_currencyyear_collection  
#> [115] met_USDexchangerate           met_GDPdeflator              
#> [117] USD_GDPpercapita              met_currencyyear_reported    
#> [119] USD_cost_total_ohd_bldg       USD_cost_total_ohd_staff     
#> [121] USD_cost_total_ohd_transport  USD_cost_total_ohd_other     
#> [123] USD_cost_total_bldgs          USD_cost_total_equip_med     
#> [125] USD_cost_total_equip_other    USD_cost_total_furniture     
#> [127] USD_cost_total_vehicles       USD_cost_total_training      
#> [129] USD_cost_total_staff_clin     USD_cost_total_staff_support 
#> [131] USD_cost_total_staff_vol      USD_cost_total_supplies_med  
#> [133] USD_cost_total_drugs          USD_cost_total_supplies_other
#> [135] USD_cost_total_maintenance    USD_cost_total_utilities     
#> [137] USD_cost_total_transport      USD_cost_total_food          
#> [139] USD_cost_total_other          USD_unitcost_ohd_bldg        
#> [141] USD_unitcost_ohd_staff        USD_unitcost_ohd_transport   
#> [143] USD_unitcost_ohd_other        USD_unitcost_bldgs           
#> [145] USD_unitcost_equip_med        USD_unitcost_equip_other     
#> [147] USD_unitcost_furniture        USD_unitcost_vehicles        
#> [149] USD_unitcost_training         USD_unitcost_staff_clin      
#> [151] USD_unitcost_staff_support    USD_unitcost_staff_vol       
#> [153] USD_unitcost_supplies_med     USD_unitcost_drugs           
#> [155] USD_unitcost_supplies_other   USD_unitcost_maintenance     
#> [157] USD_unitcost_utilities        USD_unitcost_transport       
#> [159] USD_unitcost_food             USD_unitcost_other           
#> [161] USD_unitcost_total            USD_unitcost_fixed           
#> [163] USD_unitcost_variable         USD_p_bldgspace              
#> [165] USD_p_stafftime_clin          USD_p_stafftime_nurs         
#> [167] USD_p_stafftime_pharm         USD_p_stafftime_lab          
#> [169] USD_p_stafftime_rad           USD_p_stafftime_odirect      
#> [171] USD_p_stafftime_admin         USD_p_stafftime_support      
#> [173] USD_p_stafftime_vol           LCU_tc_fixedtradeable        
#> [175] LCU_tc_fixednontradeable      LCU_uc_variabletradeable     
#> [177] LCU_uc_variablenontradeable   LCU_tc_fixedstaff            
#> [179] LCU_uc_variablestaff          private                      
#> [181] public                        urban                        
#> [183] rural                         logVisitsPP_TB               
#> [185] logVisitsPP                   logVisits                    
#> [187] logVisitsTB                   logGDP                       
#> [189] log_USD_p_bldgspace           healthcentre                 
#> [191] primary                       secondary                    
#> [193] tertiary                      USD_unitcost_ohd             
#> [195] includes_q_clin               includes_training            
#> [197] hospital                      met_PPP                      
#> [199] ID_GDPpercapita               ID_unitcost_variable         
#> [201] ID_unitcost_fixed             ID_unitcost_total            
#> [203] ID_p_bldgspace                ID_unitcost_bldgs            
#> [205] ID_unitcost_staff_clin        ID_unitcost_staff_support    
#> [207] ID_unitcost_staff_vol         ID_unitcost_training         
#> [209] ID_unitcost_utilities         ID_unitcost_ohd_bldg         
#> [211] ID_unitcost_ohd_staff         log_ID_p_bldgspace           
#> [213] ID_unitcost_equip_med         ID_unitcost_equip_other      
#> [215] ID_unitcost_furniture         ID_unitcost_vehicles         
#> [217] ID_unitcost_supplies_med      ID_unitcost_drugs            
#> [219] ID_unitcost_supplies_other    ID_unitcost_transport        
#> [221] ID_unitcost_ohd_transport     ID_unitcost_food             
#> [223] ID_unitcost_ohd_other         ID_unitcost_other            
#> [225] ID_unitcost_maintenance       ID_unitcost_ohd              
#> [227] ID_cost_total_equip_med       ID_cost_total_equip_other    
#> [229] ID_cost_total_furniture       ID_cost_total_vehicles       
#> [231] ID_CHEpercapita               fc_init_MDRTB                
#> [233] fc_init_DSTB                  fc_init_LTBI                 
#> [235] fc_init_total                 ID_unitcost_nontradeable     
#> [237] construction_idx             
#> <0 rows> (or 0-length row.names)
```
