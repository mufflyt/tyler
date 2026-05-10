# Volatile audit fields excluded from artifact_id computation

The canonical list of audit JSON fields that change between runs and are
therefore excluded when computing `artifact_id`. Any field that appears
here must NOT influence the content-addressable identity of an artifact.

## Usage

``` r
.audit_volatile_fields
```

## Format

An object of class `character` of length 7.

## Details

Keeping this constant in one place ensures that
[`mysterycall_verify_artifact()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_verify_artifact.md)
and
[`mysterycall_clean_phase1()`](https://mufflyt.github.io/mysterycall/reference/mysterycall_clean_phase1.md)
use identical canonicalization logic.
