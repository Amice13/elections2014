Results of the Ukrainian presidential elections in 2014
---------------------------------------------------

This interactive application allows anyone to explore the features of past elections of the President of Ukraine.
I have developed several interesting materials those you can study in detail:

1. Scatterplot that shows coordinates of all polling stations.
2. Histogram of results for each of the candidates and comparative histograms.
3. Scatterplot that shows relationship between results of candidates, turnout, etc.
4. Generalized by county data in tabular form.

If you want to explore the features of the voting process you can use special filters. You can limit results by selecting polling stations by their size, time of submission of result protocols and the polling station location.

I would be grateful for your comments, feedback and suggestions.

Data was gathered from the Official site of Central Election Commission [1] with Python programming language [2].
Adresses of precinct electoral comissions were geocoded with Batch Geocoding Service by David B. Zwiefelhofer [3].
This application was built on R programming language [4] with packages "Shiny" [5], "markdown" [6] and "ggplot2" [7].

The application was developed under the MIT License and can be freely forked on [GitHub](https://github.com/Amice13/elections2014).

Head of analytical department in [Promedia Consulting](https://pro-media.com.ua),

[Zakharov Kyrylo](https://www.linkedin.com/profile/view?id=25055508&trk=spm_pic)

___

### References

1. Official site of Central Election Commission. Access date: 16.06.2014. URL [http://www.cvk.gov.ua/vnd_2012_en/](http://www.cvk.gov.ua/vnd_2012_en/). 
2. G. van Rossum, Python tutorial, Technical Report CS-R9526, Centrum voor Wiskunde en Informatica (CWI), Amsterdam, May 1995.
3. Batch Geocoding. Access date: 16.06.2014. URL [http://www.findlatitudeandlongitude.com/batch-geocode/](http://www.findlatitudeandlongitude.com/batch-geocode/). 
4. R Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL [http://www.R-project.org/](http://www.R-project.org/)
5. RStudio and Inc. (2014). shiny: Web Application Framework for R. R package version 0.10.0. URL [http://CRAN.R-project.org/package=shiny](http://CRAN.R-project.org/package=shiny)
6. JJ Allaire, Jeffrey Horner, Vicent Marti and Natacha Porte (2013). markdown: Markdown rendering for R. R package version 0.6.3. URL [http://CRAN.R-project.org/package=markdown](http://CRAN.R-project.org/package=markdown)
7. H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.

___
