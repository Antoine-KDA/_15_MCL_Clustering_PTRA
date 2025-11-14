# PTRA Experiment Scripts - Complete Set
# =====================================

## Overview
Seven experiment scripts based on the README specifications (rows 6-43).
All experiments use the same core parameters with variations in:
- minPatients (30 or 50)
- pfilters and tfilters
- Data files (all vs lymph only)

## Quick Reference Table

| Script | Name | Description | minPat | pfilters | tfilters | Data Files |
|--------|------|-------------|--------|----------|----------|------------|
| experiment_generic_full_population_analysis | GEN1 | Full cohort - no restriction | 30 | id | id | all |
| experiment_gen2_full_population_analysis | GEN2 | Full cohort - no restriction | 50 | id | id | all |
| experiment_gen3_one_neoplasm_analysis | GEN3 | Full pop - one neoplasm | 50 | id | cat:[neoplasm] | all |
| experiment_gen4_controls_only_analysis | GEN4 | Full pop - controls only | 50 | control:[yes] | id | all |
| experiment_gen5_lymph_only_analysis | GEN5 | Full pop - lymph only | 50 | control:[no] | id | all |
| experiment_gen6_lymph_up_to_entry_analysis | GEN6 | Lymph pop - up to entry | 50 | [eoi+],control:[no] | id | lymph |
| experiment_gen7_full_up_to_entry_analysis | GEN7 | Full pop - up to entry | 50 | id | id | all |

## Common Parameters (All Experiments)
```
--nofAgeGroups 6
--lvl 2
--maxYears 40
--minYears 0.25
--maxTrajectoryLength 5
--minTrajectoryLength 3
--iter 500
--cluster
--mclPath /usr/bin/mcl
--clusterGranularities 40,60,80,100
```

## Data Files Used

### Full Population Experiments (GEN1-5, GEN7)
- Patient File: `ptra_all_patients_MAIN.csv`
- Diagnosis File: `ptra_all_diagnosis_MAIN.csv`

### Lymph Population Experiment (GEN6)
- Patient File: `ptra_lymph_patients_MAIN.csv`
- Diagnosis File: `ptra_lymph_diagnosis_MAIN.csv`

## Output Directories
Each experiment creates its own output directory:
- GEN1: `experiment_gen1_full_pop_results/`
- GEN2: `experiment_gen2_full_pop_results/`
- GEN3: `experiment_gen3_one_neoplasm_results/`
- GEN4: `experiment_gen4_controls_only_results/`
- GEN5: `experiment_gen5_lymph_only_results/`
- GEN6: `experiment_gen6_lymph_up_to_entry_results/`
- GEN7: `experiment_gen7_full_up_to_entry_results/`

## Running Experiments

### On Linux Server
```bash
# Make all executable
chmod +x experiment_gen*

# Run individual experiments
./experiment_generic_full_population_analysis    # GEN1
./experiment_gen2_full_population_analysis        # GEN2
./experiment_gen3_one_neoplasm_analysis           # GEN3
./experiment_gen4_controls_only_analysis          # GEN4
./experiment_gen5_lymph_only_analysis             # GEN5
./experiment_gen6_lymph_up_to_entry_analysis      # GEN6
./experiment_gen7_full_up_to_entry_analysis       # GEN7
```

### Using Job Management System
```bash
# Modify rj_run_job_nohup to point to desired experiment
# Change SCRIPT_NAME variable to one of:
# - experiment_generic_full_population_analysis
# - experiment_gen2_full_population_analysis
# - experiment_gen3_one_neoplasm_analysis
# - experiment_gen4_controls_only_analysis
# - experiment_gen5_lymph_only_analysis
# - experiment_gen6_lymph_up_to_entry_analysis
# - experiment_gen7_full_up_to_entry_analysis

./rj_run_job_nohup
```

## Parameter Differences Detailed

### GEN1 (Baseline)
- minPatients: 30 (lowest threshold)
- Filters: No restrictions (id/id)
- Purpose: Maximum inclusion

### GEN2 (Stricter baseline)
- minPatients: 50 (higher threshold)
- Filters: No restrictions (id/id)
- Purpose: More robust patterns

### GEN3 (Neoplasm filter)
- tfilters: `cat:[neoplasm]`
- Purpose: Trajectories with at least one neoplasm

### GEN4 (Controls only)
- pfilters: `control:[yes]`
- Purpose: Analyze control group trajectories

### GEN5 (Lymphoma patients only)
- pfilters: `control:[no]`
- Purpose: Analyze lymphoma patient trajectories

### GEN6 (Lymphoma up to entry date)
- Data: Lymph-specific files
- pfilters: `[eoi+],control:[no]`
- Purpose: Pre-lymphoma diagnosis trajectories

### GEN7 (Full population up to entry)
- Same as GEN2 but analyzes up to entry date
- Purpose: Overall trajectories up to index date

## Log Files
Each experiment generates timestamped logs:
- `experiment_gen1_full_pop_job_YYYYMMDD_HHMMSS.log`
- `experiment_gen2_full_pop_job_YYYYMMDD_HHMMSS.log`
- etc.

## Archive Files
Compressed results for each experiment:
- `experiment_gen1_full_pop_results_YYYYMMDD_HHMMSS.tar.gz`
- `experiment_gen2_full_pop_results_YYYYMMDD_HHMMSS.tar.gz`
- etc.

## Prerequisites
1. MCL clustering tool installed (`/usr/bin/mcl`)
2. Go programming language
3. PTRA source code at `$HOME/projects/ptra_who/ptra`
4. Data files at `$HOME/projects/data/`
5. Sufficient disk space (experiments can be large)

## Estimated Runtime
Each experiment: 2-5 days depending on data size and parameters

## Monitoring
Use the job management scripts:
```bash
./rj_quick_check          # Fast status
./rj_check_status         # Detailed status
./rj_watch_experiment     # Live monitoring
```

Created by: Antoine KDA
Date: November 2025
