import pandas as pd
import re

def split_into_sentences(text: str) -> list[str]:
    """ source: https://stackoverflow.com/questions/4576077/how-can-i-split-a-text-into-sentences –> quickly modified for Polish, the results are not great, but still better than the original corpus"""
    """
    Split the text into sentences.

    If the text contains substrings "<prd>" or "<stop>", they would lead 
    to incorrect splitting because they are used as markers for splitting.

    :param text: text to be split into sentences
    :type text: str

    :return: list of sentences
    :rtype: list[str]
    """
    alphabets= "([A-Za-z])"
    prefixes = "((P|p)rof||(K|k)mdr.|(P|p)or|(D|d)r|(K|k)s)[.]"
    suffixes = "(Inc|Ltd|Jr|Sr|Co)"
    abbr = "(ust|art|itd|itp|kw|tys|ul|proc|np|in|ub|os)"
    acronyms = "([A-Z][.][A-Z][.](?:[A-Z][.])?)"
    websites = "[.](pl|com|net|org|io|gov|edu)"
    digits = "([0-9])"
    multiple_dots = r'\.{2,}'
    text = " " + text + "  "
    text = text.replace("\n"," ")
    text = re.sub(prefixes,"\\1<prd>",text)
    text = re.sub(websites,"<prd>\\1",text)
    text = re.sub(digits + "[.]" + digits,"\\1<prd>\\2",text)
    text = re.sub(multiple_dots, lambda match: "<prd>" * len(match.group(0)) + "<stop>", text)
    text = re.sub("\s" + alphabets + "[.] "," \\1<prd> ",text)
    text = re.sub(acronyms+" "+abbr,"\\1<stop> \\2",text)
    text = re.sub(alphabets + "[.]" + alphabets + "[.]" + alphabets + "[.]","\\1<prd>\\2<prd>\\3<prd>",text)
    text = re.sub(" "+suffixes+"[.] "+abbr," \\1<stop> \\2",text)
    text = re.sub(" "+suffixes+"[.]"," \\1<prd>",text)
    text = re.sub(alphabets + "[.]" + alphabets + "[.]","\\1<prd>\\2<prd>",text)
    text = re.sub(" "+suffixes+"[.] "+abbr," \\1<stop> \\2",text)
    text = re.sub(" "+suffixes+"[.]"," \\1<prd>",text)
    text = re.sub(" " + alphabets + "[.]"," \\1<prd>",text)
    if "”" in text: text = text.replace(".”","”.")
    if "\"" in text: text = text.replace(".\"","\".")
    if "!" in text: text = text.replace("!\"","\"!")
    if "?" in text: text = text.replace("?\"","\"?")
    text = text.replace(".",".<stop>")
    text = text.replace("?","?<stop>")
    text = text.replace("!","!<stop>")
    text = text.replace("<prd>",".")
    sentences = text.split("<stop>")
    sentences = [s.strip() for s in sentences]
    if sentences and not sentences[-1]: sentences = sentences[:-1]
    return sentences

def create_csv(file):
    corpus = pd.read_json(file) #loading dataset
    #print(corpus["corpus"][0])
    texts = [] #temporary variable for storing longer texts
    df = pd.DataFrame({"sentence":[]})

    for text in corpus["corpus"]:
        texts.append(text['raw_text']) #extracting texts from the dataset
    #print(texts[:5])
    #print(len(texts))

    for text in texts: #creating dataframe with examples
        sentences = split_into_sentences(text)
        for sentence in sentences:
            if len(sentence)>15:
                df = df._append({"sentence":sentence}, ignore_index=True)
    print(df.head)
    df.to_csv("raw_sentences.csv")

def extract_patterns(input, pattern, output):
    df = pd.read_csv(input)
    df[df.sentence.str.contains(pattern)].to_csv(output)

if __name__=="__main__":
    #create_csv("corpus.json") #it was done earlier, so it's commented
    patterns = ["ystk?a[\s|.|,|?|!]", "(l|g)o(g|(żka))[\s|.|,|?|!]", "((o|e|a)w)|(el)|(ad)c(a|zyni)[\s|.|,|?|!]"] #first iteration: ["(gra|filozo)f(ka)?[\s|.|,|?|!]", "(l|g)o(g|(żka))?[\s|.|,|?|!]" – this one was wrong: unnecessary "?", "yw(ka)?[\s|.|,|?|!]", "onom(ka)?[\s|.|,|?|!]", "mistrz(yni)?[\s|.|,|?|!]", "sędzi(a|(ni)|(ina))[\s|.|,|?|!]", "cieśla[\s|.|,|?|!]", "(a|e)trk?a[\s|.|,|?|!]", "opedk?a[\s|.|,|?|!]", "(((a|e)u)|e|i|(on))tk?a[\s|.|,|?|!]" – this one backfired because of "kobieta" and "aut(k)a"]
    for i, pattern in enumerate(patterns):
        print(pattern)
        extract_patterns("raw_sentences.csv", pattern, f"sentences_{i+10}.csv")