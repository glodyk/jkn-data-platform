![Platform Type](https://img.shields.io/badge/type-data--platform-blue)
![Environment](https://img.shields.io/badge/env-production-green)
---

## Data Warehouse Architecture

The BigQuery warehouse follows a layered architecture:

raw → staging → mart → mart_prod → audit

### Layer Description

- **raw**: direct ingestion from BPJS claim sources
- **staging**: cleaned and normalized intermediate tables
- **mart**: analytic-ready datasets
- **mart_prod**: validated production datasets
- **audit**: governance and DDL activity logs

---

## Governance

This project follows strict warehouse governance principles:

- All DDL (CREATE / REPLACE) statements are stored in Git
- All mart creation activity is logged in `audit.mart_ddl_log`
- Production tables are separated from development marts
- No direct edits are performed in production datasets

The objective is to maintain reproducibility, traceability, and audit integrity.

# BPJS Kediri Data Connector

This repository is the operational data connector for the JKN analytics research environment.

It is responsible for:

* Authenticating to Google Cloud (BigQuery)
* Executing the data warehouse pipeline
* Building analytic datasets used by research projects
* Exporting audit datasets for field auditors

This repository **does not contain research analysis**.
All statistical models and research outputs live in separate repositories.

---

## How the system works

This project runs the data platform pipeline:

Local R environment → BigQuery warehouse → Analytic marts → CSV audit export (Cloud Storage)

The warehouse SQL transformation logic is maintained in:

`bpjs-chronic-analytics` repository.

This repository only executes the pipeline.

---

## Running the pipeline

1. Open the R project (`bpjs_kediri.Rproj`)
2. Authenticate to Google Cloud
3. Run:

```
source("pipeline/run_data_platform.R")
```

The pipeline will:

* Build raw tables
* Build staging tables
* Build analytic marts
* Generate audit indicators
* Export audit CSV files to Cloud Storage

---

## Security

No patient-level data is stored in this repository.

Authentication credentials are excluded using `.gitignore`.

---

## Related repositories

* `bpjs-chronic-analytics` – data warehouse SQL transformations
* `jkn-obstetric-fraud-detection` – obstetric claims research
* `jkn-strategic-purchasing-ews` – early warning system modelling

---

## Research Ecosystem

This repository is part of a larger JKN research framework:

* **jkn-claims-warehouse**: transforms raw claims into analytic datasets
* **jkn-obstetric-fraud-detection**: applies the framework to detect abnormal delivery patterns
* **jkn-strategic-purchasing-ews**: early warning indicators for purchasing and policy monitoring

The repositories are intentionally separated to distinguish infrastructure, methodology, and research applications.
