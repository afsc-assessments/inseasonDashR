SELECT
   akfin_marts.comprehensive_obs_em.obs_vessel_id,
   akfin_marts.comprehensive_obs_em.retrieval_start_date,
   akfin_marts.comprehensive_obs_em.retrieval_end_date,
   akfin_marts.comprehensive_obs_em.agency_gear_ID,
   akfin_marts.comprehensive_obs_em.obs_gear_code,
   akfin_marts.comprehensive_obs_em.obs_vessel_type,
   akfin_marts.comprehensive_obs_em.reporting_area_code,
   akfin_marts.comprehensive_obs_em.retrieval_end_latitude_dd,
   akfin_marts.comprehensive_obs_em.retrieval_end_longitude_dd,
   akfin_marts.comprehensive_obs_em.target_fishery_code,
   akfin_marts.comprehensive_obs_em.year,
   akfin_marts.comprehensive_obs_em.obs_species_code,
   akfin_marts.comprehensive_obs_em.extrapolated_weight_mt,
   akfin_marts.comprehensive_obs_em.haul_target_name,
   akfin_marts.comprehensive_obs_em.trip_target_name
FROM
    akfin_marts.comprehensive_obs_em
WHERE 1=1
AND akfin_marts.comprehensive_obs_em.obs_species_code 
-- insert species
AND akfin_marts.comprehensive_obs_em.year
-- insert year

