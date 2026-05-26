# KMextract

Automated Kaplan-Meier curve digitisation and coordinate extraction from published survival plots.

## What it does

- Converts PDF pages to SVG using MuPDF `mutool`
- Detects axes, panels, and curve geometry from vector-first plot content
- Falls back to PDF text extraction for calibration when SVG text is absent
- Produces curve coordinates suitable for downstream IPD reconstruction workflows

## Run

```bash
Rscript km_pdf_vector_extract_ultra.R --help
```

The repo currently ships the extraction engine and E156 bundle; example assets live under `_svgs/` and `test-001.svg`.

## Test

```bash
python -m pytest -q
```

The test surface is a lightweight contract layer that keeps the E156 bundle repo-relative and blocks hardcoded local-machine paths from re-entering the extractor script.

## Repo layout

| Path | Purpose |
|---|---|
| `km_pdf_vector_extract_ultra.R` | single-file KM extraction engine |
| `e156-submission/` | E156 micro-paper bundle |
| `E156-PROTOCOL.md` | project metadata (E156 entry #259) |
| `_svgs/`, `_svgs_tmp/`, `test-001.svg` | SVG fixtures and extraction artifacts |

## License

See `LICENSE`, `LICENSE.md`, or `LICENSE.txt` when present in the published bundle.
