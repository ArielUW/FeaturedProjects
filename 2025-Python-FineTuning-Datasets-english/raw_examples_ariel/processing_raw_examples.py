import pandas as pd
import re
from sentence_splitter import split_text_into_sentences
import csv

def initialize_csv(output_path):
    with open(output_path, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=['type', 'source_sentence', 'target_sentence'])
        writer.writeheader()

def extract_examples_from_csv(input_file, output_path):
    with open(input_file, newline='') as f:
        reader = csv.reader(f)
        rows = list(reader)
    for row in rows[1:]:
        sentences = split_text_into_sentences(text=row[1], language='pl')
        for sentence in sentences:
            data = {'type':0, 'source_sentence':f"{sentence}", 'target_sentence':f"{sentence}"}
            export_data(output_path,data)

def extract_examples_from_txt(input_file, output_path):
    with open(input_file, newline='') as f:
         lines = f.readlines()
    for line in lines:
         if len(line)>1:
            sentences = split_text_into_sentences(text=line, language='pl')
            for sentence in sentences:
                data = {'type':0, 'source_sentence':f"{sentence}", 'target_sentence':f"{sentence}"}
                export_data(output_path,data)

def export_data(output_path, data):
    with open(output_path, mode='a', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=['type', 'source_sentence', 'target_sentence'], escapechar='\\', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(data)

def wikipedia(input_files):    
    output_path = "./cleaned_examples_ariel/wikipedia.csv"
    initialize_csv(output_path)
    for input_file in input_files:
        extract_examples_from_csv(input_file, output_path)

def nowela(input_files):
    output_path = "./cleaned_examples_ariel/nowela.csv"
    initialize_csv(output_path)
    for input_file in input_files:
        extract_examples_from_txt(input_file, output_path)

if __name__=="__main__":
     wiki_input_files = ["raw_examples_ariel/Wikipedia_out_1.csv", "raw_examples_ariel/Wikipedia_out_2.csv", "raw_examples_ariel/Wikipedia_out_3.csv"] #sentences containing job titles were manually removed from this file
     nowela_input_files = ["raw_examples_ariel/nasza_szkapa.txt", "raw_examples_ariel/kamizelka.txt"]
     nowela(nowela_input_files)