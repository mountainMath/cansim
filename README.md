# cansim
Wrapper to access CANSIM data

Inspired by the [CANSIM2R package](https://cran.r-project.org/web/packages/CANSIM2R/index.html), with a
couple of modifications to better fit my needs. Tables are cached for the duration of the session to
avoid waits when recompiling code, the "raw" option is the default and variables get automatically read
as numeric/character as appropriate. On read it uses "Windows-1254" ecoding to avoid issues with some labels messing
with R. The package also contains a convenience function that will re-scale and
re-label variables that are reported in thousands or millions.

I contains a stub for support for french language data, although the code still faces issues with encodings.
