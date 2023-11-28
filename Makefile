

img/kelp_protection_realm_country.png: scripts/plots.R data/output/intersected_kelp_mpa_ecoregion.rds
		cd $(<D);Rscript $(<F)

data/output/intersected_kelp_mpa_ecoregion.rds: scripts/ecoregion_intersections.R data/processed/clean_mpas.gpkg data/processed/clean_kelp.gpkg data/raw/clean_meow.gpkg 
		cd $(<D);Rscript $(<F)

data/processed/clean_kelp.gpkg: scripts/clean_kelp.R data/raw/kelp/07-27-23
		cd $(<D);Rscript $(<F)

data/processed/clean_mpas.gpkg: scripts/clean_mpas.R data/raw/mpas/Final_MPAs_07-23
		cd $(<D);Rscript $(<F)
