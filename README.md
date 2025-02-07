# DEI Language Check
The R script `FrequencyCheck.R` processes a PDF then counts all occurrences of DEI keywords and phrases from [US Senate Committee on Commerce, Science, and Transit's DEI analysis of NSF grants ](https://www.commerce.senate.gov/services/files/4BD2D522-2092-4246-91A5-58EEF99750BC).

We check Flathead Valley Community College's history, mission, and strategic plan for DEI buzz words as an example; see the results in `data/MissionDeiCount.csv`

The script `WokeDF.R` compiles the keywords from Appendix B in the document above (stored in .txt files in `data/keywords`) into a data frame, saved as `data/WokeWords.Rda`. 