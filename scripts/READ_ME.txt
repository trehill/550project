READ ME: SCRIPTS 

01_species_key.R
	This script creates a key that links each species code (in presence absence matrix 
	of raw data) to associated species based on DFO report 
	
	inputs: 
	"BHM_habitat.csv" - raw data
	
	
	outputs: 
	"key.csv" - a dataset that has rows of each 'coded' species that is manually 
				edit to associate each 'NN' code to a species name based on DFO report
	"algae_key.csv" - manually made key of all algal species codes
	"invert_key.csv	- manually made key of all invert species codes		
	full_key.csv - key of both invert + algal species combined
	"missing_key.csv" - species codes that are missing to be investigated!
	"full_key_final.csv" - full CLEAN key
	
	
02_exploratory.R 
	This script does some exploratory analysis of our data by making bar and box plots 
	for substrate, algal, invert and vegetative cover metrics.
	
	inputs: 
	"BHM_habitat.csv" - raw data
	"full_key_final.csv" - key
	
	outputs: 
	substrate_bar.png - bar chart of substrates
	ubsubstrate_stackedbar.png - stacked bar chart of substrate subclasses
	algal_phylum.png - boxplot of phylums of algae
	invert_phylum.png - boxplot of phylums of inverts
	veg_cover.png - boxplot of percentage cover by vegetative category
	
	
03_richness.R
	This script makes new species richness metrics to process our data. 
	Different variables calculated based on raw data

	#Species richness per quadrat (each row of df)
	#Species richness per transect (multiple rows of df)
	#Algal species richness 
	#Invert species richness
	#Kelp presence (binary, y/n)
	#Species richness of kelp ONLY
	#Green urchin presence
	#Purple urchin presence
	#Urchin presence (all)
	
	inputs: 
	"full_key_final.csv" - key
	"BHM_habitat.csv" - raw data
	
	
	outputs: 
	"processed_data.csv" - data with additional columns (see above)
	
04_transect.R 
	This script aggregates the data to the Transect level (instead of quadrant). We add other information 
	like Year, and average depth to be used as fixed + random effects later

	inputs: 
	processed_data.csv
	BHM_RA.csv - includes year and depth data 


	outputs: 
	"transect.csv"

04_models.R 
	This script is creates mixed effect models for kelp and urchins

	inputs: 
	"transect_final.csv"

	outputs: 
	plots of odd ratios for kelp + urchins
	