# Snapshot: normalized audit structure is stable

    Code
      names(normalized)
    Output
       [1] "artifact_id"            "cohort_hash"            "empty_names_count"     
       [4] "function_name"          "input_colnames"         "input_cols"            
       [7] "input_npi_count"        "input_rows"             "no_last_name_count"    
      [10] "original_npi_preserved" "output_cols"            "output_rows"           
      [13] "quality_metrics"        "rows_duplicated"        "rows_retained_pct"     
      [16] "schema_version"        

# Snapshot: quality_metrics keys are stable

    Code
      sort(names(audit$quality_metrics))
    Output
      [1] "completeness_names"   "completeness_npi"     "completeness_phone"  
      [4] "has_processing_flags"

# Snapshot: schema_version value is locked

    {
      "type": "character",
      "attributes": {},
      "value": ["1.2.0"]
    }

