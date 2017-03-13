#!/usr/bin/env python3
# Adapted from https://gist.github.com/alexhanna/56ac247ca65d9d9b2b8a
import re
import csv
from datetime import datetime

# ---------------------
# Variables to modify
# ---------------------
raw_input = '2016-3.TXT'
clean_output = '2016-3.csv'

# Identify metadata sections
metadata_sections = ['LENGTH', 'HEADLINE', 'GEOGRAPHIC', 'BODY']


# ---------------
# Actual script
# ---------------
# Read the file
ln_raw = open(raw_input, 'r').read()

# Find the end of the article based on the copyright notice through 'All Rights Reserved'
workfile = ln_raw.replace('\ufeff', '')
workfile = re.sub(r'\d+ of \d+ DOCUMENTS', 'ENDOFILE', workfile)
workfile = re.sub(r'Copyright .*?(.*\n){1,4}.*All Rights Reserved', '', workfile)

# Edge cases
workfile = re.sub(r'Copyright .*?(.*\n){1,4}.*Provided by Syndigate Media Inc\.', '', workfile)
workfile = workfile.replace('Distributed by Tribune Content Agency', '')

workfile_all = workfile.split('ENDOFILE')

articles_all = []

for article in workfile_all[1:]:
    article_dict = {}

    # Split the article into lines
    article_lines = [line.strip() for line in article.split('\n\n') if len(line.strip()) > 0]
    # print(article_lines)

    article_dict['PUBLICATION'] = article_lines[0]

    # Split date into chunks and only use the first three
    # No need for the weekday
    date_raw = [date_chunk.strip() for date_chunk in article_lines[1].split(' ')]
    # print(date_raw)
    date = datetime.strptime(' '.join(date_raw[0:3]).replace(',', ''), "%B %d %Y")
    date = date.strftime("%Y-%m-%d")
    article_dict['DATE'] = date

    # Recombine the cleaned article so we can split it by metadata HEADINGS:
    article_rebuilt = '\n'.join(article_lines[2:])

    # Prefix each of the predefined metadata sections with ZZZZ for later splitting
    article_rebuilt = re.sub('(' + ':|'.join(metadata_sections) + ':)', 'ZZZZ\\1', article_rebuilt)
    article_split = [line.strip() for line in article_rebuilt.split('ZZZZ') if len(line.strip()) > 0]

    for meta in metadata_sections:
        # Extract the list elements that match the current metadata
        chunk_raw = [chunk for chunk in article_split if chunk.startswith(meta + ':')]

        # Convert extracted elements to string
        chunk_clean = '\n'.join(chunk_raw)

        # Remove headers from string
        chunk_clean = re.sub(':|'.join(metadata_sections) + ':', '', chunk_clean).strip()

        # Save cleaned chunk to dictionary
        if len(chunk_clean) > 0:
            article_dict[meta] = chunk_clean
        else:
            article_dict[meta] = None

    # Append to big list
    articles_all.append(article_dict)


# Write everyting to CSV
with open(clean_output, 'w') as csvfile:
    fieldnames = ['PUBLICATION', 'DATE'] + metadata_sections
    w = csv.DictWriter(csvfile, fieldnames=fieldnames)

    w.writeheader()

    for article in articles_all:
        w.writerow(article)
