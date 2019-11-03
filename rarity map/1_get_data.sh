
# filter for 4.1 
SELECT observation_type, plot_name,
scrubbed_species_binomial, 
latitude, longitude, country, continent, 
is_geovalid 
FROM view_full_occurrence_individual 
WHERE scrubbed_species_binomial IS NOT NULL 
AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') 
AND (is_introduced = 0 OR is_introduced IS NULL) 
AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) 
AND is_location_cultivated IS NULL 
AND observation_type IN ('plot','specimen') 
limit 10;


#-2018-11-27, query
\copy (SELECT observation_type, datasource,plot_name, scrubbed_species_binomial, latitude, longitude, country, continent, is_geovalid FROM view_full_occurrence_individual WHERE scrubbed_species_binomial IS NOT NULL AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND (is_introduced = 0 OR is_introduced IS NULL) AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) AND is_location_cultivated IS NULL AND observation_type IN ('plot','specimen') ) TO 'bien4.1.0_rarity_20181127.tsv'WITH (FORMAT CSV, DELIMITER E'\t', HEADER TRUE, FORCE_QUOTE *);


