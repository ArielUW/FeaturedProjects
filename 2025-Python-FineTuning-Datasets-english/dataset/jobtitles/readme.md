## Dataset structure

Dataset has a simple structure. It contains CSV files with 3 columns each:
    type,source_sentence,target_sentence
separated by commas.

There are 2 distinct example types (1 and 0) which are described below.

All of the examples have been shuffled at two distinct stages:
* after all the examples have been sorted by type, but before the splitting (to ensure random distribution of differnet sources over each split),
* after the data was split (to ensure random order of 0s and 1s witin each split).

### Type 1

Those are the sentences that contain nouns designating job titles. In the [source_sentence] column, the job title is gendered. In the [target_sencence] column, the gendered job title has been replaced with so-called "personative" (see below).

An effort has been made to pick examples containing only one job title per sentence, preferebly in nominative case of the singular number, and to avoid including edge cases. However, those criteria are not met for every type 1 sentence in the set.

_Semantic_ edge cases include personal nouns that are similar to job titles but are not job titles per se, eg. [terrorysta], [kolekcjoner], [anarchista], [zwolennik]. In general, they are nouns which describe people in relation to their activities, interests or political/buisness/organizational affiliations. Some semantic edge cases have been kept, eg. [kierowca].
_Morphological_ edge cases include job titles that are very hard to neutralise, eg. [listonosz].

### Type 0

Those are the sentences that *do not* contain any job titles. Both sentences in the same row ([source_sentence] and [target_sentence] columns) are exactly the same.

Sentences with job titles or semantic/morphological edge cases have been removed or edited, so that they do not contain words such as:
* [gitarzyst(k)a], [pianist(k)a], [wokalist(k)a] and other names of musicians, hobbyists
* [partner(ka)], [członek/członkini], [przedstawiciel(k)a] and other names denoting people from the POV of political or business relationships
* [mieszkaniec/mieszkanka] and names denoting people by ethnicity, nationality or locality

Some nouns designating persons were left. Typye 0 sentences may still contain some gendered and non-gendered personal nouns, such as:
* [kobieta], [mężczyzna]
* [matka], [ojcicec], [rodzic], [syn], [córka], [dziecko]
* [mąż], [żona], [małżonek], [małżonka]
* [osoba], [człowiek], [ludzie]
* [pan], [pani], [państwo]
* [bóg], [bogini], [bóstwo]

### Splits

The dataset is split into 3 parts with the following amount of example sentences:

split \ example |  type 1 |  type 0 |  TOTAL  
---------------------------------------------
train.csv       |     610 |    4221 |   4831
validation.csv  |      50 |     500 |    250
test.csv        |     250 |     250 |    500
---------------------------------------------
TOTAL           |     910 |    4971 |   5881

## Sources

Type 1 examples come from the _NKJP corpus_ enriched with a small amount targeted searches in _Google_ (estimated amount of examples is 20-60). Some sentences have been altered so that better meet the criteria.

Type 0 examples mostly come from _Wikipedia_ targeted scraping made with [WikipediaScraper.py] from [https://github.com/UW-IMLLA/week-7-ArielUW/tree/main/pipeline], as well as from 2 short novels __Nasza szkapa__ and __Kamizelka__ published in _Wolnelektury_. All of them have been manually edited to make sure there are no sentences containing job titles or semantic/morphological edge cases. Some residual sentcences from _NKJP_ have been added – they were irrelevant search reasults obtained while looking for type 1 examples with the use of regex.

SOURCE          |  type 1 |  type 0 |   TOTAL 
----------------------------------------------
Wikipedia       |       0 |    3830 |    3830 
Short novels    |       0 |    1100 |    1100   
NKJP            | 850-890 |      41 | 891-931
Google          |   20-60 |       0 |   20–60

### Splitting source texts into example sentences

Most of the rows contain single sentences. However, some of the automatic or manual delimitations may contain errors. In other cases sentence delimitation may be controversial (especially for literary texts or spoken language transcripts).

Sentences from _Wikipedia_ and from _WolneLektury_ have been split into sentences using _Sentence Splitter_ Python library: [https://github.com/mediacloud/sentence-splitter]. Sentence splitting of _WolneLektury_ was additionally corrected manually.
Sentences from _NKJP_ have been split into sentences in majority using _Spacy_ library (without corrections), while a small amount was split using an ad-hoc heuristic method combined with plenty of manual corrections.
Sentences found with the help of _Google Search Engine_ have been manually copied and did not need anny automated processing.