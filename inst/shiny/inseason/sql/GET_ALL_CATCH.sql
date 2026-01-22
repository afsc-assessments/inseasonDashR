SELECT
    council.comprehensive_blend_ca.year,
    council.comprehensive_blend_ca.reporting_area_code,
    council.comprehensive_blend_ca.week_end_date,
    council.comprehensive_blend_ca.fmp_gear,
    council.comprehensive_blend_ca.agency_species_code,
    council.comprehensive_blend_ca.weight_posted,
    council.comprehensive_blend_ca.species_name
FROM
    council.comprehensive_blend_ca
WHERE
    1 = 1
AND council.comprehensive_blend_ca.agency_species_code
-- insert species
AND  council.comprehensive_blend_ca.year
-- insert year