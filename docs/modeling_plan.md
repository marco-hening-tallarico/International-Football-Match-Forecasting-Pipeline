# Modeling Plan

## Objective

Produce calibrated multiclass probabilities P(H), P(D), P(A) for international matches using only pre-match information.

## Stages

| Stage | Script | Question answered |
|-------|--------|-------------------|
| Baseline | 19 | How strong are trivial and Elo-only models? |
| Feature audit | 20, 22 | Which columns are safe to model? |
| EDA | 23 | Target balance, missingness, drift |
| Safe ML | 24 | Do glmnet / LightGBM beat Elo on approved features? |
| Draw diagnostics | 25 | Why is draw hard to rank? |
| Draw-aware Elo | 26 | Do nonlinear Elo transforms help? |
| Lagged form | 27, 28 | Does recent form add signal? |
| Goalscorers | 29, 30 | Does attacking depth add beyond form? |
| Final report | 31, 32 | Consolidated comparison and narrative |

## Feature tiers (incremental)

1. **baseline_rating** — Elo difference only  
2. **rating_plus_context** — + tournament / neutral flags  
3. **rating_plus_form** — + compact lagged form  
4. **rating_plus_form_plus_goalscorers** — + goalscorer depth  

Each tier uses the same chronological splits for fair comparison.

## Model families per stage

- **19:** frequency, majority, `multinom` on Elo  
- **24+:** `multinom`, `glmnet` (multinomial ridge), `lightgbm` (if installed)  
- Complete-case cohort within each feature variant (counts logged per script)

## Selection protocol

1. Train on train split.  
2. Evaluate all candidates on validation split.  
3. Select lowest validation log loss.  
4. Report test metrics once for the selected configuration.  

No test-set peeking during feature or model selection.

## Non-goals (this project version)

- Hyperparameter grid search at production scale  
- Neural networks or embedding-based team models  
- In-play or live updating  
- Betting market integration (documented as future work)
