# ADR-20251005: Transform v1 - Wave Grouping and Scenario Handling

## Context
- Wikipedia polling table contains multiple scenarios per poll wave (full-field, adams_removed, head-to-head variants)
- Need model-ready dataset with one "primary" row per wave for Bayesian time-series modeling
- Must preserve all rows for sensitivity analysis and alternate scenarios
- Date range spans May-September 2025 with Eric Adams withdrawal on 2025-09-28

## Decision
- **Wave identifier**: `pollster_wave_id = {pollster_slug}_{YYYYmmdd}` using date_median
- **Primary selection rule**: Per wave, prefer full_field scenario → largest sample_size
- **Two outputs**: (1) ALL cleaned rows for diagnostics, (2) PRIMARY rows (one per wave) for modeling
- **Scenario encoding**: Deterministic typing (full_field, adams_removed, head_to_head_*) from raw row_type
- **Candidate set tracking**: `candidates_in_poll` = sorted concat for potential fixed effects (kept for diagnostics, not used in v1 model)
- **CLI-first design**: All paths configurable via flags; `--dryrun` (default TRUE) prevents accidental overwrites

## Alternatives Considered
- Use `response_id` as wave key (not available in polling data; would be ideal for panel data)
- Flatten scenarios into wide columns per wave (loses scenario granularity; harder to model conditional on scenario)
- Prefer LV > RV > A for primary selection (user feedback: prefer full_field scenario instead; vote_status kept for analysis)
- Hardcode file paths (rejected: makes pipeline brittle and non-reproducible)

## Consequences
- **Pro**: Clear wave deduplication enables `(1 | pollster_wave_id)` random effects in brms
- **Pro**: Deterministic primary selection = reproducible training data
- **Pro**: All scenarios preserved for alternate model specifications
- **Pro**: Dryrun flag enables safe development and testing
- **Con**: Primary selection rule discards potentially higher-quality LV samples if full_field is RV
- **Mitigation**: Keep vote_status in data; can filter/reweight in modeling stage
- **Con**: Wave ID collision if same pollster releases two polls on same median date (unlikely but possible)
- **Mitigation**: Manual inspection during EDA; add sequence suffix if needed
