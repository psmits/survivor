Data files:

Sampledinbin_PaleoDB.txt - data file including size, age, and calculated metabolic rates for each genus sampled within a given time interval in the Paleobiology Database.

Occurrence_PaleoDB.txt - data file including size, age, locality, abundance and calculated metabolic rate information for bivalves and brachiopods in the Paleobiology Database.

Rangethrough_allsources.txt - data file including size, age, and calculated metabolic rates for each genus in each time interval using the maximum stratigraphic range reported across all sources (earliest first occurrence and latest last occurrence)

Rangethough_PaleoDB.txt - data file including size, age, and calculated metabolic rates for each genus in each time interval using the earliest first occurrence and latest last occurrence of each genus in the Paleobiology Database

Abundance_PaleoDB.txt - data file including collection and abundance information for bivalves and brachiopods

BivalveBrach-refs.tab - file including list of all primary source references for data used from the Paleobiology Database

delta_T.txt - data file including differences in global mean temperature relative to present, used in calculating temperature-adjusted metabolic rates

feeding.txt - data file of feeding mode for each genus

###########################################################################
Sampledinbin_PaleoDB.txt 

Data fields:
taxon_name: name of the genus
int_midpoint: midpoint of the geological stage in millions of years before present
taxon: clade to which the genus belongs - Biv = Bivalvia, Bra=Brachiopoda
sub: subgroup to which the genus belongs - Het = heterbranch bivalve, nonHet = all other bivalves, art = articulate brachiopod, inert = inarticulate brachiopod
size: maximum linear dimension in mm.


###########################################################################
Occurrence_PaleoDB.txt

Data fields:
taxon_name: name of the genus
pbdb_collection_no: collection number in the Paleobiology Database
p_lat: paleolatitude for the collection from Paleobiology Database
p_lng: paleolongitude for the collection from the Paleobiology Database
int_midpoint: midpoint of the geological stage in millions of years before present
taxon: clade to which the genus belongs - Biv = Bivalvia, Bra=Brachiopoda
sub: subgroup to which the genus belongs - Het = heterbranch bivalve, nonHet = all other bivalves, art = articulate brachiopod, inert = inarticulate brachiopod
size: maximum linear dimension in mm


###########################################################################
Rangethrough_allsources.txt 

Data fields:
taxon_name: name of the genus
int_midpoint: midpoint of the geological stage in millions of years before present
taxon: clade to which the genus belongs - Biv = Bivalvia, Bra=Brachiopoda
sub: subgroup to which the genus belongs - Het = heterbranch bivalve, nonHet = all other bivalves, art = articulate brachiopod, inert = inarticulate brachiopod
size: maximum linear dimension in mm.


###########################################################################
Rangethough_PaleoDB.txt

Data fields:
taxon_name: name of the genus
int_midpoint: midpoint of the geological stage in millions of years before present
taxon: clade to which the genus belongs - Biv = Bivalvia, Bra=Brachiopoda
sub: subgroup to which the genus belongs - Het = heterbranch bivalve, nonHet = all other bivalves, art = articulate brachiopod, inert = inarticulate brachiopod
size: maximum linear dimension in log10 mm.


###########################################################################
Abundance_PaleoDB.txt

Data fields:
taxon_name: name of the genus
int_midpoint: midpoint of the geological stage in millions of years before present
phylum: Linnean phylum
class: Linnean class
pbdb_collection_no: collection number in the Paleobiology Database
p_lat: paleolatitude from Paleobiology Database
p_lng: paleolongitude from Paleobiology Database
int_midpoint: midpoint of the geological stage in millions of years before present
pbdb_abundance: abundance reported from Paleobiology Database
pbdb_abund_unit: units for pbdb_abundance field


###########################################################################
feeding.txt

Data fields:
taxon_name: name of the genus
phylum: Linnean phylum
class: Linnean class
feeding: feeding mode
	1 - suspension feeder
	2 - surface deposit feeder
	3 - sediment miner
	4 - grazer
	5 - predator
	6 - other


###########################################################################
delta_T.txt

Data fields:

int_midpoint: midpoint of the geological stage in millions of years before present
delta_T: difference between interval global mean temperature and present day global mean temperature, in degrees Celsius