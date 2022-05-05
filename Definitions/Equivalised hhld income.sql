/**************************************************************************************************
Title: Add equivalised household income to the assembled household data
Author: Freya Li
Reviewer: Simon Anastasiadis

Inputs & Dependencies:
- [IDI_Sandpit].[DL-MAA2020-01].[d2gP2_rectangular_hhld]

Outputs:
- [IDI_Sandpit].[DL-MAA2020-01].[d2gP2_rectangular_hhld_EquInc]


Description:
We adjust disposable household income for household size and composition to allow living standards
to be compared across households.


Intended purpose:
Adding variable equivalised household income to identify low income household 


Notes:
1. Two diffrent equivalence scales has been created:
   (1) Using a single-person household as a base, assigns a value of 1 to the first adult 
   in the household, 0.5 to each additional adult member, and 0.3 to each child.
   (2) Square root scale: taking the square root of the number of people in the household.

2. Equivalised household income:
   Devide household income by the scale factor calculated for each household to give equivalised
   household income.

3. Low income household: Less than 50% median equivalised household income. (will be defined in
   tidy_variables_hhld.R) 

Issues and limitations:

Reference: https://www.stats.govt.nz/assets/Uploads/Methods/Measuring-child-poverty-concepts-and
-definitions/measuring-child-poverty-concepts-and-definitions.pdf

Parameters & Present values:
  Prefix = d2gP2_
  Project schema = [DL-MAA2020-01]

 

History:
2021-09-20 SA review
2021-07-15 FL
**************************************************************************************************/

IF OBJECT_ID('[IDI_Sandpit].[DL-MAA2020-01].[d2gP2_rectangular_hhld_EquInc]','U') IS NOT NULL
DROP TABLE [IDI_Sandpit].[DL-MAA2020-01].[d2gP2_rectangular_hhld_EquInc];
GO
SELECT *
	   ,ROUND(total_income_ben/scale_factor1, 2) AS equ_hhld_income1
	   ,ROUND(total_income_ben/scale_factor2, 2) AS equ_hhld_income2
INTO [IDI_Sandpit].[DL-MAA2020-01].[d2gP2_rectangular_hhld_EquInc]
FROM(
SELECT *
       ,1 + 0.5*(size_hhld - child_num_hhld - 1) + 0.3*child_num_hhld AS scale_factor1
       ,SQRT(size_hhld) AS scale_factor2
FROM  [IDI_Sandpit].[DL-MAA2020-01].[d2gP2_rectangular_hhld]
) a





