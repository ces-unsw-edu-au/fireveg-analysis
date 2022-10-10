#!R --vanilla

require(RpostgreSQL)

qry <- 'SELECT family AS fam,count(distinct "speciesID"), count(distinct s.species_code), count(distinct q.species_code) FROM species.caps LEFT JOIN species_traits s ON "speciesCode_Synonym"=s.species_code::text LEFT JOIN form.quadrat_samples q ON "speciesCode_Synonym"=q.species_code::text  GROUP BY fam';
