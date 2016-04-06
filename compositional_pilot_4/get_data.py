from sqlalchemy import create_engine, MetaData, Table
import json
import pandas as pd


header = ['id', 'itemTime', 'language', 'trial', 'antonymPair', 'scenarioLabel',
          'predicateType',
          'labelLower', 'labelHigher',
          'expressionLabel1', 'expression1',
          'expressionLabel2', 'expression2',
          'expressionLabel3', 'expression3',
          'expressionLabel4', 'expression4',
          'expressionLabel5', 'expression5',
          'expressionLabel6', 'expression6',
          'expressionLabel7', 'expression7',
          'answer1',
          'answer2',
          'answer3',
          'answer4',
          'answer5',
          'answer6',
          'answer7',
          'RT', 'engagement', 'difficulty', 'comments']

db_url = 'sqlite:///participants.db'
table_name = 'rating'
data_column_name = 'datastring'

# boilerplace sqlalchemy setup
engine = create_engine(db_url)
metadata = MetaData()
metadata.bind = engine
table = Table(table_name, metadata, autoload=True)

# make a query and loop through
s = table.select()
rows = s.execute()

data = []
#status codes of subjects who completed experiment
statuses = [3,4,5,7]
# if you have workers you wish to exclude, add them here
exclude = []
for row in rows:
    # only use subjects who completed experiment and aren't excluded
    if row['status'] in statuses and row['uniqueid'] not in exclude:
        data.append(row[data_column_name])

# Now we have all participant datastrings in a list.
# Let's make it a bit easier to work with:

# parse each participant's datastring as json object
# and take the 'data' sub-object
data = [json.loads(part)['data'] for part in data]

# insert a few things into the data array
for part in data:
    for record in part:
        trialdata = record['trialdata']
        try:
            trialdata.insert(0, record['dateTime'])
            trialdata.insert(0, record['uniqueid'])
        except AttributeError:
            continue 

# flatten nested list so we just have a list of the trialdata recorded
# each time psiturk.recordTrialData(trialdata) was called.
data = [record['trialdata'] for part in data for record in part
                       if isinstance(record['trialdata'], list)]

def addLanguage(data):
    newdat = []

    j = 0
    part = list(set([data[i][0] for i in range(len(data))]))
    meta = [line for line in data if len(line) <= 3]

    for i in range(len(part)):
        curpart = part[i]

        language = meta[j][-1]
        comments = meta[j+1][-1]
        engagement = meta[j+2][-1]
        difficulty = meta[j+3][-1]

        for line in data:
            if line[0] != curpart or len(line) <= 3:
                continue

            extended = line[:]
            extended.insert(2, language)
            extended.insert(len(extended), engagement)
            extended.insert(len(extended), difficulty)
            extended.insert(len(extended), comments)

            newdat.append(extended)

        j+= 4

    return newdat

data = addLanguage(data)

# Put all subjects' trial data into a dataframe object from the
# 'pandas' python library: one option among many for analysis
data_frame = pd.DataFrame(data)
data_frame.to_csv('data.csv', header = header, index = False)
