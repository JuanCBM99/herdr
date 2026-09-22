# Changelog

## herdr 1.0.3

### New Features

- Full integration of monogastric animals (swine and poultry) using
  FEDNA energy requirement equations.
- Automatic herd demographic modeling for swine (breeding sows,
  replacement, fattening) and poultry (broilers, layer hens, breeders).
- Tier 1 enteric fermentation methane calculation with liveweight
  scaling for swine.
- Land use allocation module with FAO trade matrix tracing and
  self-sufficiency ratio (SSR) thresholds.
- Interactive Shiny graphical user interface with cascading dropdowns,
  dynamic validation, and automated report generation.

### Bug Fixes and Improvements

- Isolated test environments and anti-download shields for offline
  execution.
- Scaled small ruminant nitrogen retention to intake according to
  GLEAM/IPCC standards.
- Refined input validation for manure management combinations and diet
  profiles.
