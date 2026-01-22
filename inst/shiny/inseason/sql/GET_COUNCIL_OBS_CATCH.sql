SELECT
    	council.comprehensive_obs.haul_join,
    	council.comprehensive_obs.obs_vessel_id,
    	council.comprehensive_obs.obs_gear_code,
    	council.comprehensive_obs.total_hooks_pots,
    	council.comprehensive_obs.year,
    	council.comprehensive_obs.duration_in_min,
    	council.comprehensive_obs.extrapolated_weight,
    	council.comprehensive_obs.extrapolated_number,    
    	council.comprehensive_obs.fmp_area,
    	council.comprehensive_obs.fmp_subarea,
    	council.comprehensive_obs.FMP_GEAR,
    	council.comprehensive_obs.VES_AKR_LENGTH,
    	council.comprehensive_obs.week_end_date,
		council.comprehensive_obs.trip_target_code,
        council.comprehensive_obs.trip_target_name,
        council.comprehensive_obs.target_fishery_name
    	to_char(council.comprehensive_obs.week_end_date,'mm') as MONTH
FROM
    	council.comprehensive_obs
WHERE
    	council.comprehensive_obs.year 
		--insert year 
    	AND council.comprehensive_obs.obs_specie_code 
		--insert species 
  


