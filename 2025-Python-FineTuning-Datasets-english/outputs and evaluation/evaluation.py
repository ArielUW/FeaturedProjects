from Levenshtein import distance
import pandas as pd
import os

""" METRICS
Mean Levenshtein distance for Type 1 examples
    – threshold = 0.6
    – formula:
    SG = gold-standard sentence from the test dataset
    Sp = predicted sentence
    E1 = number of rows with type 1 examples in the test dataset
    MeanLev = Σlev(SG, SP) / E1
    
Mean Levenshtein distance for Type 0 examples
    – threshold = 0.8
    – formula:
    SG = gold-standard sentence from the test dataset
    Sp = predicted sentence
    E0 = number of rows with type 0 examples in the test dataset
    MeanLev = Σlev(SG, SP) / E1"""

def split_outputs(file):
    try:
        df = pd.read_csv(file, sep=";")
    except:
        try:
            df = pd.read_csv(file, sep=",")
        except Exception as e:
            print(f"Unable to convert {file} into dataframe: {e}")
    if "type" in df.columns:
        try:
            one = df.loc[(df['type'] == 1)]
            zero = df.loc[(df['type'] == 0)]
        except:
            print(f"Invalid 'type' values in {file}. Use 1 and 0.")
    else:
        print(f"Table {file} is missing 'type' column: impossible to split by type.")
    return one, zero

def normalized_distance(s1, s2): # i had to write a custom function, because there was something wrong with normalized=True in my built-in one
    return distance(s1,s2) / max(len(s1), len(s2))

def add_lev(df):
    try:
        df["lev"]= df.apply(lambda x: normalized_distance(x["target_sentence"], x["generated_text"]), axis = 1)
    except Exception as e:
        print(f"Impossible to calculate Levensthein distance for 'target_sentence' and 'generated_text' in this dataframe: {e}")

def mean(series):
    return series.sum()/len(series)

def export_worst_examples(df, output_folder):
    if df.loc[:,"type"].iloc[0]==0:
        name = "zero"
    elif df.loc[:,"type"].iloc[0]==1:
        name = "one"
    else:
        name = "untitled"
    worst = df[df.lev>0]
    worst.sort_values("lev", ascending=False)
    worst.to_csv(f"{output_folder}/worst_{name}.csv")

def export_levs_to_csv(df, output_folder):
    if df.loc[:,"type"].iloc[0]==0:
        name = "zero"
    elif df.loc[:,"type"].iloc[0]==1:
        name = "one"
    else:
        name = "untitled"
    df.to_csv(f"{output_folder}/{name}_with_levs.csv")

if __name__=="__main__":
    os.chdir="/Users/arieldrozd/Downloads/IMLLA-FinalProject"
    input_file = "outputs and evaluation/generated_job_titles.csv"
    output_folder="outputs and evaluation/evaluation"
    dfs = split_outputs(input_file)
    for df in dfs:
        add_lev(df)
        export_worst_examples(df, output_folder)
        export_levs_to_csv(df, output_folder)
        print(mean(df["lev"]))