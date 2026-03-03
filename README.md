![Platform Type](https://img.shields.io/badge/type-data--platform-blue)
![Environment](https://img.shields.io/badge/env-production-green)
---

# BPJS Kediri Data Connector
Part of the JKN Data Platform ecosystem.

---

## Overview

This repository is the operational data connector for the JKN analytics research environment.

It is responsible for:

- Authenticating to Google Cloud (BigQuery)
- Executing the data warehouse pipeline
- Building analytic datasets used by research projects
- Exporting audit datasets for field auditors

This repository **does not contain research analysis**.
All statistical models and research outputs live in separate repositories.

---

## System Architecture

The JKN data platform follows a structured warehouse design:

raw → staging → mart → mart_prod → audit

### Layer Description

- **raw**: Source claims data (nonkapitasi, inacbgs, etc.)
- **staging**: Cleaned and standardized intermediate tables
- **mart**: Analytic datasets for research and monitoring
- **mart_prod**: Production-stable datasets
- **audit**: Governance and DDL tracking tables

All DDL definitions are version-controlled in Git.

All CREATE/REPLACE operations are logged in:

`bpjs-kediri.audit.mart_ddl_log`

---

## How the System Works

Pipeline flow:

Local R environment → BigQuery warehouse → Analytic marts → CSV audit export (Cloud Storage)

The warehouse SQL transformation logic is maintained in:

`bpjs-chronic-analytics` repository.

This repository executes the pipeline only.

---

## Running the Pipeline

1. Open the R project (`bpjs_kediri.Rproj`)
2. Authenticate to Google Cloud
3. Run:

```
source("pipeline/run_data_platform.R")
```

The pipeline will:

- Build raw tables
- Build staging tables
- Build analytic marts
- Generate audit indicators
- Export audit CSV files to Cloud Storage

---

## Governance

This repository follows warehouse governance principles:

- All DDL stored in Git
- Production tables separated (`mart` vs `mart_prod`)
- Automated DDL logging via scheduled query
- Version tagging for stable releases

---

## Security

- No patient-level data is stored in this repository
- Authentication credentials excluded via `.gitignore`
- Access controlled via Google Cloud IAM

---

## Related Repositories

- `bpjs-chronic-analytics` – warehouse SQL transformations
- `jkn-obstetric-fraud-detection` – obstetric claims research
- `jkn-strategic-purchasing-ews` – early warning system modelling
- `jkn-claims-warehouse` – transforms raw claims into analytic datasets

---

## Research Ecosystem

The repositories are intentionally separated to distinguish:

- Infrastructure
- Methodology
- Research applications

This separation ensures reproducibility, governance, and security.