import pandas as pd

# Read dataframes using pandas

yearJoined = pd.read_csv('data/year_joined.csv')
donations = pd.read_csv( 'data/donations.csv')
emails = pd.read_csv('data/emails.csv')

print(yearJoined.groupby('memberid').count().groupby('memberStats').count())
# 1000, czyli
# Czyli kazdy user ma jeden status. Tabela nie trzyma historii

print(emails[emails.emailsOpened < 1])
# Empty - fakt. Nie ma tygodnia z otwartymi emailami rownymi 0

# Czy sa dziury ? Czy tygodnie z mailami nie odczytanymi sa nie rejestrowane ?