# UZIS data analysis
## Data inputs
### Data on vaccination: (source: UZIS via Vesely)
- Vesely_106_202403141131.csv
### Demografic data: (source: Czech Statistical Office)
- 130055230807.xlsx 
- DEMD003-CR-F.xlsx
- DEMD003-CR-M.xlsx

## Procedure
There are 3 steps. Steps 2 and 3 can be run without need to run previous steps.
1.  Transforming original csv file with vaccination records into `UZISdata.RData`
    - `Vesely_106_202403141131.csv` -> `01-data_wrangling_part1.R` -> UZISdata.RData (lasts several minutes)
2. Tranfroming data into two files: `At_Risk.csv` and `Died.csv`
    - `UZISdata.RData` -> `02A-data_wrangling_agregation_At_Risk.R` -> `At_Risk.csv` (several hours!)
    - `UZISdata.RData` -> `02B-data_wrangling_agregation_Died.R` -> `Died.csv` (few minutes)
3. Analysis
	- `At_Risk.csv` + `Died.csv` + `130055230807.xlsx` + `DEMD003-CR-F.xlsx` + `DEMD003-CR-M.xlsx` -> `umrtnostni_tabulky.R` + `computation.R` --> `03-central_file.R` -> all csv files such as group_born_...-F.csv, group_born_...-M.csv or with prefix CZ (the same files using middle-European separators)	

