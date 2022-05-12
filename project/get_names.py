import numpy as np
import pandas as pd
import spacy
from collections import Counter

#%% 
# Load package to parse texts
nlp = spacy.load("en_core_web_lg")

texts = pd.read_csv("tango_texts.csv", index_col=(0))
tweets = list(texts['x'])
docs = list(nlp.pipe(tweets))

#%% 
# Collect names from each Tweet
all_names = []

for doc in docs:
    names = [i for i in doc.ents if i.label_ == "PERSON"]
    if len(str(names)) > 2:
        all_names.append(str(names)[1:-1])
        
# Separate multiple names in single Tweet
ext_names = []

for names in all_names:
    name = names.split(",")
    ext_names.extend(name)

# Clean up the names, filter out hyperlinks
clean_names = []
for name in ext_names:
    clean = name.strip()
    if clean[:8] != 'https://':
        clean_names.append(clean)

#%% 
# Use Counter to aggregate name counts
clean_count = Counter(clean_names)

# Convert to pandas and write out to CSV
df_clean = pd.DataFrame(clean_count.most_common(), columns=['name','count'])
df_clean.to_csv('clean_counts.csv')

