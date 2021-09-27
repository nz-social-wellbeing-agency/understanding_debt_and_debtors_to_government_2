/**************************************************************************************************
Title: 2020 Residential population
Author: Simon Anastasiadis
Re-edit: Freya Li
Reviewer: Simon Anastasiadis

Inputs & Dependencies:
- [IDI_Clean].[data].[snz_res_pop]
Outputs:
- [IDI_UserCode].[DL-MAA2020-01].[d2gP2_2019_2020_residents]

Description:
List of snz_uid values for those identities that are part of the
Estimated Residential Population (ERP) of New Zealand in 2020.

Intended purpose:
Producing summary statistics for the entire population.

Notes:

Parameters & Present values:
  Current refresh = 20201020
  Prefix = d2gP2_
  Project schema = [DL-MAA2020-01]
   
Issues:
 
History (reverse order):
2021-02-02 FL v3 Put year 2019 and 2020 into one single population difinition
2021-01-26 SA QA
2021-01-11 FL v2 (Change prefix, update the table to the latest refresh, update date)
2020-07-14 MP QA
2020-03-03 SA v1
**************************************************************************************************/
/* Establish database for writing views */
USE IDI_UserCode
GO
IF OBJECT_ID('[DL-MAA2020-01].[d2gP2_2019_2020_tmp_residents]','V') IS NOT NULL
DROP VIEW [DL-MAA2020-01].[d2gP2_2019_2020_tmp_residents];
GO

CREATE VIEW [DL-MAA2020-01].[d2gP2_2019_2020_tmp_residents] AS
SELECT [snz_uid]
	  ,srp_ref_date
FROM [IDI_Clean_20201020].[data].[snz_res_pop]
WHERE YEAR(srp_ref_date) = 2019
OR YEAR(srp_ref_date) = 2020;
GO

--keep only one record fore ach identity
IF OBJECT_ID('[DL-MAA2020-01].[d2gP2_2019_2020_residents]','V') IS NOT NULL
DROP VIEW [DL-MAA2020-01].[d2gP2_2019_2020_residents];
GO
CREATE VIEW [DL-MAA2020-01].[d2gP2_2019_2020_residents] AS
SELECT [snz_uid]      
FROM [DL-MAA2020-01].[d2gP2_2019_2020_tmp_residents]
GROUP BY snz_uid
GO

--remove temp view
IF OBJECT_ID('[DL-MAA2020-01].[d2gP2_2019_2020_tmp_residents]','V') IS NOT NULL
DROP VIEW [DL-MAA2020-01].[d2gP2_2019_2020_tmp_residents];
GO