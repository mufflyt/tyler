# Primary outcome analysis for mystery caller studies

Two complementary functions cover the headline results every mystery
caller paper reports: appointment wait times and appointment acceptance
rates. Both accept an optional grouping column (typically `"insurance"`)
and return a named list containing a tidy summary tibble, the raw test
object, a p-value, and a plain-language interpretation sentence.
