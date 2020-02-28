# a function used to write a csv file that has a list of all unique species
# and the species corresponding counts

def counts_of_species(filename):

    data = pd.read_csv(filename)

    species_counts = (data['taxon_species_name'].value_counts()).to_dict()

    with open('species_counts.csv', 'w', newline = '') as csvfile:
        writer = csv.writer(csvfile)
        for species, counts in species_counts.items():
            writer.writerow([species, counts])

import pandas as pd
import csv

counts_of_species('iNaturalist-birds-LARW.csv')
