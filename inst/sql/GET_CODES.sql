SELECT
    norpac_views.akr_species_code_conversion.obs_program_code,
    norpac_views.akr_species_code_conversion.obs_program_name,
    norpac_views.akr_species_code_conversion.akr_program_code,
    norpac_views.akr_species_code_conversion.akr_program_name
FROM
    norpac_views.akr_species_code_conversion
WHERE norpac_views.akr_species_code_conversion.obs_program_code
-- insert species
ORDER BY
    norpac_views.akr_species_code_conversion.obs_program_code