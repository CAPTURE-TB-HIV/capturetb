# Get raw data filtered by output type

Returns the raw ValueTB data filtered to a specified output type.

## Usage

``` r
get_data(output_name = NULL, output_group = NULL)
```

## Arguments

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
#>   fc_country fc_code fc_geog1            fc_type fc_ownership fc_size_m2
#> 1   Ethiopia     ET3    Urban  Tertiary hospital       Public   16156.01
#> 2   Ethiopia     ET3    Urban  Tertiary hospital       Public   16156.01
#> 3   Ethiopia     ET4    Urban Secondary hospital       Public    8764.65
#> 4   Ethiopia    ET22    Urban  Tertiary hospital       Public    5548.00
#> 5   Ethiopia    ET22    Urban  Tertiary hospital       Public    5548.00
#> 6   Ethiopia    ET25    Urban Secondary hospital       Public    1265.00
#>   fc_opvisits_total fc_opvisits_TB fc_ipdays_total fc_ipdays_tb ss_lab_total
#> 1            253130         188517           31800        23760       113693
#> 2            253130         188517           31800        23760       113693
#> 3            506517         175578           37315         2156       217680
#> 4            549075         431162           35023        14880       140014
#> 5            549075         431162           35023        14880       140014
#> 6            119293          72285           31832         2744        84204
#>   ss_lab_tb ss_rad_total ss_rad_tb  capacity fc_catchmentpop fc_timespent_TB
#> 1     59172        19022       856 0.6061670         3500000        51.98933
#> 2     59172        19022       856 0.6061670         3500000        51.98933
#> 3     41405         5243       133 0.8249114         2700000        33.17158
#> 4     65428        12155      2886 0.8543480         3500000        65.88287
#> 5     65428        12155      2886 0.8543480         3500000        65.88287
#> 6      8282        14492      9368 0.5351271          600000        18.69946
#>   fc_FTE_facility_clin fc_FTE_facility_support fc_FTE_total  fc_m2_TB
#> 1                  572                     735         1307 3785.9492
#> 2                  572                     735         1307 3785.9492
#> 3                  426                     308          734 1294.3678
#> 4                  643                     288          931 2725.4516
#> 5                  643                     288          931 2725.4516
#> 6                  302                     176          478  578.7743
#>   fc_beds_facility fc_beds_TB fc_FTE_TB_doctors fc_FTE_TB_nurses
#> 1              360         36         31.120167        34.024467
#> 2              360         36         31.120167        34.024467
#> 3              211         59          7.072000         9.784667
#> 4              179         22         24.094462        22.399867
#> 5              179         22         24.094462        22.399867
#> 6              249         16          9.617011        10.821333
#>   fc_FTE_TB_pharm fc_FTE_TB_lab fc_FTE_TB_rad fc_FTE_TB_odirect fc_FTE_TB_admin
#> 1       13.260000      8.200400     0.5148000        2.40933333        4.420000
#> 2       13.260000      8.200400     0.5148000        2.40933333        4.420000
#> 3        1.967333      5.135000     0.7691667        7.33720000        7.800000
#> 4        6.581033     11.610300     2.5896000       14.83300000        3.179628
#> 5        6.581033     11.610300     2.5896000       14.83300000        3.179628
#> 6        4.141867      2.593067     1.6682667        0.08746667        4.669867
#>   fc_FTE_TB_support fc_FTE_TB_vol fc_FTE_TB outputgroup          output
#> 1          13.26000             0 107.20917          IP ip_ipbedday_mdr
#> 2          13.26000             0 107.20917          IP  ip_ipbedday_ds
#> 3          10.40000             0  50.26537          IP ip_ipbedday_mdr
#> 4          10.40000             0  95.68789          IP ip_ipbedday_mdr
#> 5          10.40000             0  95.68789          IP  ip_ipbedday_ds
#> 6          10.08107             0  43.67994          IP ip_ipbedday_mdr
#>   originaloutputdescription servicestatistics met_stafftime_obs
#> 1      Inpatient bedday_MDR             15000                NA
#> 2       Inpatient bedday_DS              8760                NA
#> 3      Inpatient bedday_MDR              2156                NA
#> 4      Inpatient bedday_MDR             13800                NA
#> 5       Inpatient bedday_DS              1080                NA
#> 6      Inpatient bedday_MDR              2744                NA
#>   met_stafftime_int met_stafftime_tmst met_stafftime_ass met_equip_obs
#> 1            0.0625              0.125            0.8125            NA
#> 2            0.0625              0.125            0.8125            NA
#> 3            0.0000              0.000            1.0000            NA
#> 4            0.0000              0.000            0.0000            NA
#> 5            0.0000              0.000            0.0000            NA
#> 6            0.0000              0.000            0.0000            NA
#>   met_equip_int met_supp_obs met_supp_int met_TDvBU met_FINvECON q_bldgspace
#> 1            NA           NA           NA        TD         ECON   140.20946
#> 2            NA           NA           NA        TD         ECON    81.88232
#> 3            NA           NA           NA        TD         ECON    93.89563
#> 4            NA           NA           NA        TD         ECON   340.61817
#> 5            NA           NA           NA        TD         ECON    26.65707
#> 6            NA           NA           NA        TD         ECON    20.74986
#>   q_stafftime_clin q_stafftime_nurs q_stafftime_pharm q_stafftime_lab
#> 1     2400.0000000     2400.0000000                 0               0
#> 2        0.0000000        0.0000000                 0               0
#> 3        0.3246753        0.2319109                 0               0
#> 4        0.0000000        0.0000000                 0               0
#> 5        0.0000000        0.0000000                 0               0
#> 6        0.0000000        0.0000000                 0               0
#>   q_stafftime_rad q_stafftime_odirect q_stafftime_admin q_stafftime_support
#> 1               0                   0                 0                   0
#> 2               0                   0                 0                   0
#> 3               0                   0                 0                   0
#> 4               0                   0                 0                   0
#> 5               0                   0                 0                   0
#> 6               0                   0                 0                   0
#>   q_stafftime_vol LCU_p_bldgspace LCU_p_stafftime_clin LCU_p_stafftime_nurs
#> 1               0       1465.3663            0.2642308            0.2642308
#> 2               0       1465.3663            0.2642308            0.2642308
#> 3               0        419.1139            0.9874038            0.5024388
#> 4               0       1462.7225                   NA            0.1627141
#> 5               0       1462.7225                   NA            0.1627141
#> 6               0       2790.9706            0.1930769            0.2090385
#>   LCU_p_stafftime_pharm LCU_p_stafftime_lab LCU_p_stafftime_rad
#> 1             0.3016346           0.3016346           0.3016346
#> 2             0.3016346           0.3016346           0.3016346
#> 3             0.6327404           0.4552885           0.5025240
#> 4                    NA                  NA                  NA
#> 5                    NA                  NA                  NA
#> 6                    NA                  NA                  NA
#>   LCU_p_stafftime_odirect LCU_p_stafftime_admin LCU_p_stafftime_support
#> 1               0.3016346             0.3016346               0.3016346
#> 2               0.3016346             0.3016346               0.3016346
#> 3               0.5406250             0.5564615               0.1308462
#> 4                      NA                    NA                      NA
#> 5                      NA                    NA                      NA
#> 6                      NA                    NA                      NA
#>   LCU_p_stafftime_vol LCU_cost_total_ohd_bldg LCU_cost_total_ohd_staff
#> 1           0.8056731               413286.08               145356.466
#> 2           0.8056731               241359.07                84888.176
#> 3                  NA                39134.29                45159.753
#> 4                  NA               463360.15                83693.595
#> 5                  NA                36262.97                 6549.934
#> 6                  NA                65358.75                66982.819
#>   LCU_cost_total_ohd_transport LCU_cost_total_ohd_other LCU_cost_total_bldgs
#> 1                   11881.7252               2940803.35            205458.21
#> 2                    6938.9275               1717429.15            119987.59
#> 3                    4826.7786                 13234.61             39352.97
#> 4                    5367.7314                473549.28            498229.87
#> 5                     420.0833                 37060.38             38991.90
#> 6                   49334.5908                203106.12             57912.25
#>   LCU_cost_total_equip_med LCU_cost_total_equip_other LCU_cost_total_furniture
#> 1                 2315.183                   1746.837               16245.3347
#> 2                 3397.853                   1746.837                9685.5845
#> 3                 3526.385                      0.000                 550.2535
#> 4                    0.000                      0.000                   0.0000
#> 5                    0.000                      0.000                   0.0000
#> 6                    0.000                   1853.830                 668.2139
#>   LCU_cost_total_vehicles LCU_cost_total_training LCU_cost_total_staff_clin
#> 1                   0.000             414578.6299                 1232510.4
#> 2                   0.000              22313.7123                 1028690.2
#> 3               49129.779                  0.0000                   43543.5
#> 4                   0.000                  0.0000                  422678.0
#> 5                   0.000                  0.0000                   21333.0
#> 6                1149.457                203.9163                  292068.0
#>   LCU_cost_total_staff_support LCU_cost_total_staff_vol
#> 1                            0                        0
#> 2                            0                        0
#> 3                            0                        0
#> 4                            0                        0
#> 5                            0                        0
#> 6                            0                        0
#>   LCU_cost_total_supplies_med LCU_cost_total_drugs
#> 1                300929.37634                    0
#> 2                118863.92049                    0
#> 3                 18937.79380                    0
#> 4                    98.50670                    0
#> 5                     7.70922                    0
#> 6                     0.00000                    0
#>   LCU_cost_total_supplies_other LCU_cost_total_maintenance
#> 1                             0                          0
#> 2                             0                          0
#> 3                             0                          0
#> 4                             0                          0
#> 5                             0                          0
#> 6                             0                          0
#>   LCU_cost_total_utilities LCU_cost_total_transport LCU_cost_total_food
#> 1                        0                    0.000               0.000
#> 2                        0                    0.000               0.000
#> 3                        0                10051.200            7344.349
#> 4                        0                    0.000         1035546.421
#> 5                        0                    0.000           81042.763
#> 6                        0                 1469.441           36084.793
#>   LCU_cost_total_other LCU_unitcost_ohd_bldg LCU_unitcost_ohd_staff
#> 1                    0              27.55241               9.690431
#> 2                    0              27.55241               9.690431
#> 3                    0              18.15134              20.946082
#> 4                    0              33.57682               6.064753
#> 5                    0              33.57682               6.064753
#> 6                    0              23.81879              24.410648
#>   LCU_unitcost_ohd_transport LCU_unitcost_ohd_other LCU_unitcost_bldgs
#> 1                   0.792115             196.053556           13.69721
#> 2                   0.792115             196.053556           13.69721
#> 3                   2.238766               6.138503           18.25277
#> 4                   0.388966              34.315165                 NA
#> 5                   0.388966              34.315165                 NA
#> 6                  17.979078              74.018265                 NA
#>   LCU_unitcost_equip_med LCU_unitcost_equip_other LCU_unitcost_furniture
#> 1              0.1543455                0.1164558              1.0830223
#> 2              0.3878827                0.1994106              1.1056603
#> 3              1.6356147                0.0000000              0.2552196
#> 4                     NA                0.0000000              0.0000000
#> 5                     NA                0.0000000              0.0000000
#> 6                     NA                0.6755941              0.2435182
#>   LCU_unitcost_vehicles LCU_unitcost_training LCU_unitcost_staff_clin
#> 1             0.0000000           27.63857533                82.16736
#> 2             0.0000000            2.54722743               117.43039
#> 3            22.7874669            0.00000000                20.19643
#> 4             0.0000000            0.00000000                30.62884
#> 5             0.0000000            0.00000000                19.75278
#> 6             0.4188983            0.07431353               106.43878
#>   LCU_unitcost_staff_support LCU_unitcost_staff_vol LCU_unitcost_supplies_med
#> 1                          0                      0              20.061958423
#> 2                          0                      0              13.568940696
#> 3                          0                      0               8.783763359
#> 4                          0                      0               0.007138166
#> 5                          0                      0               0.007138166
#> 6                          0                      0               0.000000000
#>   LCU_unitcost_drugs LCU_unitcost_supplies_other LCU_unitcost_maintenance
#> 1                  0                           0                        0
#> 2                  0                           0                        0
#> 3                  0                           0                        0
#> 4                  0                           0                        0
#> 5                  0                           0                        0
#> 6                  0                           0                        0
#>   LCU_unitcost_utilities LCU_unitcost_transport LCU_unitcost_food
#> 1                      0              0.0000000           0.00000
#> 2                      0              0.0000000           0.00000
#> 3                      0              4.6619666           3.40647
#> 4                      0              0.0000000          75.03960
#> 5                      0              0.0000000          75.03960
#> 6                      0              0.5355106          13.15043
#>   LCU_unitcost_other LCU_unitcost_total LCU_unitcost_fixed
#> 1                  0           379.0074          276.77812
#> 2                  0           383.0252          252.02590
#> 3                  0           127.4544           90.40576
#> 4                  0           216.1249          110.44932
#> 5                  0           205.2488          110.44932
#> 6                  0           282.8689          162.74415
#>   LCU_unitcost_variable met_currency met_currencyyear_collection
#> 1             102.22932          ETB                        2018
#> 2             130.99933          ETB                        2018
#> 3              37.04863          ETB                        2018
#> 4             105.67557          ETB                        2018
#> 5              94.79951          ETB                        2018
#> 6             120.12472          ETB                        2018
#>   met_USDexchangerate met_GDPdeflator USD_GDPpercapita
#> 1           0.0365732              NA         771.5238
#> 2           0.0365732              NA         771.5238
#> 3           0.0365732              NA         771.5238
#> 4           0.0365732              NA         771.5238
#> 5           0.0365732              NA         771.5238
#> 6           0.0365732              NA         771.5238
#>   met_currencyyear_reported USD_cost_total_ohd_bldg USD_cost_total_ohd_staff
#> 1                      2018               15115.195                 5316.151
#> 2                      2018                8827.273                 3104.632
#> 3                      2018                1431.266                 1651.637
#> 4                      2018               16946.564                 3060.943
#> 5                      2018                1326.253                  239.552
#> 6                      2018                2390.379                 2449.776
#>   USD_cost_total_ohd_transport USD_cost_total_ohd_other USD_cost_total_bldgs
#> 1                    434.55273              107554.5938             7514.265
#> 2                    253.77879               62811.8828             4388.331
#> 3                    176.53075                 484.0322             1439.264
#> 4                    196.31512               17319.2129            18221.861
#> 5                     15.36379                1355.4167             1426.059
#> 6                   1804.32397                7428.2412             2118.036
#>   USD_cost_total_equip_med USD_cost_total_equip_other USD_cost_total_furniture
#> 1                 84.67365                   63.88741                594.14392
#> 2                124.27036                   63.88741                354.23282
#> 3                128.97121                    0.00000                 20.12453
#> 4                  0.00000                    0.00000                  0.00000
#> 5                  0.00000                    0.00000                  0.00000
#> 6                  0.00000                   67.80051                 24.43872
#>   USD_cost_total_vehicles USD_cost_total_training USD_cost_total_staff_clin
#> 1                 0.00000            15162.467773                45076.8516
#> 2                 0.00000              816.083923                37622.4961
#> 3              1796.83325                0.000000                 1592.5251
#> 4                 0.00000                0.000000                15458.6855
#> 5                 0.00000                0.000000                  780.2161
#> 6                42.03933                7.457872                10681.8613
#>   USD_cost_total_staff_support USD_cost_total_staff_vol
#> 1                            0                        0
#> 2                            0                        0
#> 3                            0                        0
#> 4                            0                        0
#> 5                            0                        0
#> 6                            0                        0
#>   USD_cost_total_supplies_med USD_cost_total_drugs
#> 1                1.100595e+04                    0
#> 2                4.347234e+03                    0
#> 3                6.926157e+02                    0
#> 4                3.602705e+00                    0
#> 5                2.819508e-01                    0
#> 6                0.000000e+00                    0
#>   USD_cost_total_supplies_other USD_cost_total_maintenance
#> 1                             0                          0
#> 2                             0                          0
#> 3                             0                          0
#> 4                             0                          0
#> 5                             0                          0
#> 6                             0                          0
#>   USD_cost_total_utilities USD_cost_total_transport USD_cost_total_food
#> 1                        0                  0.00000              0.0000
#> 2                        0                  0.00000              0.0000
#> 3                        0                367.60455            268.6064
#> 4                        0                  0.00000          37873.2461
#> 5                        0                  0.00000           2963.9934
#> 6                        0                 53.74217           1319.7365
#>   USD_cost_total_other USD_unitcost_ohd_bldg USD_unitcost_ohd_staff
#> 1                    0             1.0076797              0.3544101
#> 2                    0             1.0076797              0.3544101
#> 3                    0             0.6638527              0.7660653
#> 4                    0             1.2280118              0.2218074
#> 5                    0             1.2280118              0.2218074
#> 6                    0             0.8711293              0.8927755
#>   USD_unitcost_ohd_transport USD_unitcost_ohd_other USD_unitcost_bldgs
#> 1                 0.02897018              7.1703062          0.5009510
#> 2                 0.02897018              7.1703062          0.5009510
#> 3                 0.08187883              0.2245047          0.6675621
#> 4                 0.01422573              1.2550155                 NA
#> 5                 0.01422573              1.2550155                 NA
#> 6                 0.65755248              2.7070849                 NA
#>   USD_unitcost_equip_med USD_unitcost_equip_other USD_unitcost_furniture
#> 1             0.00564491              0.004259160            0.039609592
#> 2             0.01418611              0.007293083            0.040437538
#> 3             0.05981967              0.000000000            0.009334199
#> 4                     NA              0.000000000            0.000000000
#> 5                     NA              0.000000000            0.000000000
#> 6                     NA              0.024708640            0.008906240
#>   USD_unitcost_vehicles USD_unitcost_training USD_unitcost_staff_clin
#> 1            0.00000000           1.010831237               3.0051234
#> 2            0.00000000           0.093160264               4.2948055
#> 3            0.83341062           0.000000000               0.7386481
#> 4            0.00000000           0.000000000               1.1201947
#> 5            0.00000000           0.000000000               0.7224223
#> 6            0.01532045           0.002717884               3.8928068
#>   USD_unitcost_staff_support USD_unitcost_staff_vol USD_unitcost_supplies_med
#> 1                          0                      0              0.7337300181
#> 2                          0                      0              0.4962595999
#> 3                          0                      0              0.3212503493
#> 4                          0                      0              0.0002610656
#> 5                          0                      0              0.0002610656
#> 6                          0                      0              0.0000000000
#>   USD_unitcost_drugs USD_unitcost_supplies_other USD_unitcost_maintenance
#> 1                  0                           0                        0
#> 2                  0                           0                        0
#> 3                  0                           0                        0
#> 4                  0                           0                        0
#> 5                  0                           0                        0
#> 6                  0                           0                        0
#>   USD_unitcost_utilities USD_unitcost_transport USD_unitcost_food
#> 1                      0             0.00000000         0.0000000
#> 2                      0             0.00000000         0.0000000
#> 3                      0             0.17050305         0.1245855
#> 4                      0             0.00000000         2.7444382
#> 5                      0             0.00000000         2.7444382
#> 6                      0             0.01958534         0.4809535
#>   USD_unitcost_other USD_unitcost_total USD_unitcost_fixed
#> 1                  0          13.861515          10.122662
#> 2                  0          14.008459           9.217394
#> 3                  0           4.661415           3.306428
#> 4                  0           7.904379           4.039485
#> 5                  0           7.506607           4.039485
#> 6                  0          10.345420           5.952075
#>   USD_unitcost_variable USD_p_bldgspace USD_p_stafftime_clin
#> 1              3.738853        53.59314          0.009663765
#> 2              4.791065        53.59314          0.009663765
#> 3              1.354987        15.32834          0.036112521
#> 4              3.864894        53.49644                   NA
#> 5              3.467122        53.49644                   NA
#> 6              4.393346       102.07473          0.007061441
#>   USD_p_stafftime_nurs USD_p_stafftime_pharm USD_p_stafftime_lab
#> 1          0.009663765            0.01103174          0.01103174
#> 2          0.009663765            0.01103174          0.01103174
#> 3          0.018375795            0.02314134          0.01665136
#> 4          0.005950977                    NA                  NA
#> 5          0.005950977                    NA                  NA
#> 6          0.007645206                    NA                  NA
#>   USD_p_stafftime_rad USD_p_stafftime_odirect USD_p_stafftime_admin
#> 1          0.01103174              0.01103174            0.01103174
#> 2          0.01103174              0.01103174            0.01103174
#> 3          0.01837891              0.01977239            0.02035158
#> 4                  NA                      NA                    NA
#> 5                  NA                      NA                    NA
#> 6                  NA                      NA                    NA
#>   USD_p_stafftime_support USD_p_stafftime_vol LCU_tc_fixedtradeable
#> 1             0.011031743          0.02946604             20307.354
#> 2             0.011031743          0.02946604             14830.273
#> 3             0.004785463                  NA             53206.418
#> 4                      NA                  NA                 0.000
#> 5                      NA                  NA                 0.000
#> 6                      NA                  NA              3671.501
#>   LCU_tc_fixednontradeable LCU_uc_variabletradeable LCU_uc_variablenontradeable
#> 1               3986008.25                 20.06196                   0.0000000
#> 2               2108028.50                 13.56894                   0.0000000
#> 3                 96548.65                 12.19023                   4.6619668
#> 4               1440507.00                 75.04674                   0.0000000
#> 5                112735.33                 75.04674                   0.0000000
#> 6                375915.62                 13.15043                   0.5355107
#>   LCU_tc_fixedstaff LCU_uc_variablestaff private public urban rural
#> 1        145356.469             82.16736   FALSE   TRUE  TRUE FALSE
#> 2         84888.180            117.43040   FALSE   TRUE  TRUE FALSE
#> 3         45159.754             20.19643   FALSE   TRUE  TRUE FALSE
#> 4         83693.594             30.62884   FALSE   TRUE  TRUE FALSE
#> 5          6549.934             19.75278   FALSE   TRUE  TRUE FALSE
#> 6         66982.820            106.43877   FALSE   TRUE  TRUE FALSE
#>   logVisitsPP_TB logVisitsPP logVisits logVisitsTB   logGDP log_USD_p_bldgspace
#> 1       1.572264  -0.6337286  12.44166    12.14694 6.648368            3.981421
#> 2       1.572264  -0.6337286  12.44166    12.14694 6.648368            3.981421
#> 3       2.258625   0.6369068  13.13531    12.07584 6.648368            2.729703
#> 4       2.513250   0.4798337  13.21599    12.97424 6.648368            3.979615
#> 5       2.513250   0.4798337  13.21599    12.97424 6.648368            3.979615
#> 6       1.511586  -0.3801702  11.68934    11.18837 6.648368            4.625705
#>   healthcentre primary secondary tertiary USD_unitcost_ohd includes_q_clin
#> 1        FALSE   FALSE     FALSE     TRUE         8.561366            TRUE
#> 2        FALSE   FALSE     FALSE     TRUE         8.561366           FALSE
#> 3        FALSE   FALSE      TRUE    FALSE         1.736302            TRUE
#> 4        FALSE   FALSE     FALSE     TRUE         2.719061           FALSE
#> 5        FALSE   FALSE     FALSE     TRUE         2.719061           FALSE
#> 6        FALSE   FALSE      TRUE    FALSE         5.128542           FALSE
#>   includes_training hospital  met_PPP ID_GDPpercapita ID_unitcost_variable
#> 1              TRUE     TRUE 9.319892        2095.318             9.550072
#> 2              TRUE     TRUE 9.319892        2095.318            13.096234
#> 3             FALSE     TRUE 9.319892        2095.318             3.113077
#> 4             FALSE     TRUE 9.319892        2095.318             6.031094
#> 5             FALSE     TRUE 9.319892        2095.318             4.864121
#> 6              TRUE     TRUE 9.319892        2095.318            11.959014
#>   ID_unitcost_fixed ID_unitcost_total ID_p_bldgspace ID_unitcost_bldgs
#> 1         29.601821          39.15189      157.22996          1.469675
#> 2         26.921987          40.01822      157.22996          1.469675
#> 3          7.954949          11.06803       44.96982          1.958474
#> 4         11.850923          17.88202      156.94630                NA
#> 5         11.850922          16.71504      156.94630                NA
#> 6         17.367391          29.32641      299.46383                NA
#>   ID_unitcost_staff_clin ID_unitcost_staff_support ID_unitcost_staff_vol
#> 1               8.816343                         0                     0
#> 2              12.599974                         0                     0
#> 3               2.167024                         0                     0
#> 4               3.286394                         0                     0
#> 5               2.119421                         0                     0
#> 6              11.420602                         0                     0
#>   ID_unitcost_training ID_unitcost_utilities ID_unitcost_ohd_bldg
#> 1          2.965546761                     0             2.956301
#> 2          0.273310834                     0             2.956301
#> 3          0.000000000                     0             1.947591
#> 4          0.000000000                     0             3.602705
#> 5          0.000000000                     0             3.602705
#> 6          0.007973647                     0             2.555693
#>   ID_unitcost_ohd_staff log_ID_p_bldgspace ID_unitcost_equip_med
#> 1             1.0397579           5.057709            0.00564491
#> 2             1.0397579           5.057709            0.01418611
#> 3             2.2474597           3.805992            0.05981967
#> 4             0.6507321           5.055904                    NA
#> 5             0.6507321           5.055904                    NA
#> 6             2.6191986           5.701994                    NA
#>   ID_unitcost_equip_other ID_unitcost_furniture ID_unitcost_vehicles
#> 1             0.004259160           0.039609592           0.00000000
#> 2             0.007293083           0.040437538           0.00000000
#> 3             0.000000000           0.009334199           0.83341062
#> 4             0.000000000           0.000000000           0.00000000
#> 5             0.000000000           0.000000000           0.00000000
#> 6             0.024708640           0.008906240           0.01532045
#>   ID_unitcost_supplies_med ID_unitcost_drugs ID_unitcost_supplies_other
#> 1             0.7337300181                 0                          0
#> 2             0.4962595999                 0                          0
#> 3             0.3212503493                 0                          0
#> 4             0.0002610656                 0                          0
#> 5             0.0002610656                 0                          0
#> 6             0.0000000000                 0                          0
#>   ID_unitcost_transport ID_unitcost_ohd_transport ID_unitcost_food
#> 1            0.00000000                0.08499187        0.0000000
#> 2            0.00000000                0.08499187        0.0000000
#> 3            0.50021681                0.24021369        0.1245855
#> 4            0.00000000                0.04173504        2.7444382
#> 5            0.00000000                0.04173504        2.7444382
#> 6            0.05745889                1.92910802        0.4809535
#>   ID_unitcost_ohd_other ID_unitcost_other ID_unitcost_maintenance
#> 1            21.0360332                 0                       0
#> 2            21.0360332                 0                       0
#> 3             0.6586453                 0                       0
#> 4             3.6819274                 0                       0
#> 5             3.6819274                 0                       0
#> 6             7.9419660                 0                       0
#>   ID_unitcost_ohd ID_cost_total_equip_med ID_cost_total_equip_other
#> 1       25.061062                248.4130                  187.4310
#> 2       25.061062                364.5807                  187.4310
#> 3        4.935575                378.3719                    0.0000
#> 4        7.949590                  0.0000                    0.0000
#> 5        7.949590                  0.0000                    0.0000
#> 6       13.774411                  0.0000                  198.9111
#>   ID_cost_total_furniture ID_cost_total_vehicles
#> 1              1743.08187                 0.0000
#> 2              1039.23785                 0.0000
#> 3                59.04076              5271.4966
#> 4                 0.00000                 0.0000
#> 5                 0.00000                 0.0000
#> 6                71.69760               123.3337
```
