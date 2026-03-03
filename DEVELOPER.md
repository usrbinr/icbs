# STWD Developer Guide

This guide documents the architecture, design decisions, and maintenance approach for the stwd package.

## Package Overview

stwd (Storytelling with Data) provides tools for creating presentation-ready visualizations following SWD principles. The main feature is `story_designer()`, a Shiny app for interactively designing story layouts.

## File Structure

```
R/
├── blocks.R              # 448 lines - Exported block functions
├── block_helpers.R       # 138 lines - Internal helpers for blocks
├── code_generation.R     # 273 lines - Generate R code strings
├── colors.R              # 200 lines - Color utilities + package imports
├── designer_defaults.R   # 155 lines - Default values, reset functions
├── layout.R              # 281 lines - story_layout, estimate_layout_heights
├── mod_export.R          # 259 lines - Shiny module: export/download
├── mod_legend.R          # 146 lines - Shiny module: legend config
├── mod_palette.R         # 353 lines - Shiny module: palette picker
├── palette_helpers.R     #  96 lines - Palette lookup functions
├── plot_analysis.R       #  69 lines - Extract info from ggplot objects
├── story_designer.R      # 668 lines - Main Shiny app
├── story_designer_ui.R   # 285 lines - UI builder functions
├── text_helpers.R        #  78 lines - Text/marquee processing
├── theme.R               #  80 lines - theme_stwd
```

## Architecture Principles

### 1. Single Responsibility
Each file has one purpose. No "utils" dumping grounds.

### 2. Exported vs Internal
- **Exported functions**: In `blocks.R`, `colors.R`, `layout.R`, `theme.R`
- **Internal helpers**: In `*_helpers.R` files, all marked `@noRd`

### 3. Shiny Modularization
The Shiny app uses `moduleServer()` pattern:
- `mod_palette_server()` - Color palette selection
- `mod_legend_server()` - Inline legend configuration
- `mod_export_server()` - Export settings and download

## Core Components

### Block Functions (`blocks.R`)

Block functions create ggplot objects for patchwork composition:

```r
title_block()     # Large title with marquee formatting
subtitle_block()  # Supporting context
caption_block()   # Source attribution (gray by default)
text_narrative()  # Side panel narrative text
legend_block()    # Inline colored text legend
```

All block functions:
1. Call `check_marquee()` to verify marquee is available
2. Use `create_text_block()` for consistent rendering
3. Return a ggplot object compatible with patchwork

**Adding a new block function:**
```r
my_block <- function(text, size = 10, ...) {
    check_marquee("my_block")

    create_text_block(
        text = text,
        size = size,
        halign = "left",
        y_pos = 0.5,
        vjust = 0.5,
        ...
    )
}
```

### Block Helpers (`block_helpers.R`)

Internal functions that support blocks:

| Function | Purpose |
|----------|---------|
| `check_marquee()` | Abort if marquee not installed |
| `get_hjust()` | Convert "left"/"center"/"right" to 0/0.5/1 |
| `get_x_pos()` | Convert alignment to x coordinate |
| `get_vjust()` | Convert "top"/"center"/"bottom" to 1/0.5/0 |
| `get_y_pos()` | Convert alignment to y coordinate |
| `create_text_block()` | Core function that builds the ggplot |
| `as_block()` | Convert text or ggplot to block (for story_layout) |

### Text Helpers (`text_helpers.R`)

Functions for processing marquee-formatted text:

| Function | Purpose |
|----------|---------|
| `convert_named_colors()` | `{red text}` → `{#FF0000 text}` |
| `maybe_wrap_text()` | Apply strwrap if wrap_width specified |
| `strip_marquee_formatting()` | Remove `**` and `{#hex ...}` for char counting |
| `color_to_hex()` | Convert R color name to hex code |

### Layout Functions (`layout.R`)

**`estimate_layout_heights()`** - Calculates height proportions based on text length:
- Strips marquee formatting to count actual characters
- Estimates line wrapping based on font size and output width
- Returns proportions that sum to 1.0

**`story_layout()`** - Composes a complete story layout:
1. Converts text to block plots using `as_block()`
2. Positions narrative (right/left/bottom)
3. Stacks components with patchwork
4. Applies height proportions

### Color Functions (`colors.R`)

**`list_colors()`** - Browse R color names with hex equivalents

**`highlight_colors()`** - Create strategic highlighting:
```r
# Gray out everything except highlighted categories
highlight_colors(c("A", "B", "C"), highlight = "B")
# Returns: A="#D3D3D3", B="#1E90FF", C="#D3D3D3"
```

**`inline_legend()`** - Create data frame for marquee legend annotation

### Theme (`theme.R`)

**`theme_stwd()`** - Clean SWD-style theme:
- White background, no border
- Horizontal grid lines only
- No axis lines or ticks
- Legend at top-left

## Shiny App Architecture

### Main App (`story_designer.R`)

Structure:
1. **UI definition** - Uses `bslib::page_sidebar()` with accordion panels
2. **Server function** - Orchestrates modules and reactive rendering
3. **Module calls** - Delegates to `mod_*_server()` functions

Key reactives:
- `title_text_d()`, `subtitle_text_d()`, etc. - Debounced text inputs
- `current_heights()` - Layout height proportions
- `build_layout()` - Main composition reactive

### Modules

**`mod_palette_server()`** returns:
```r
list(
    package = reactive(...),      # Selected package name
    palette = reactive(...),      # Selected palette name
    colors = reactive(...),       # Vector of hex colors
    current_palette = reactive(...),
    manual_enabled = reactive(...),
    manual_colors = reactive(...)
)
```

**`mod_legend_server()`** returns:
```r
list(
    enabled = reactive(...),
    plot = reactive(...),        # legend_block() output
    colors = reactive(...),      # Named color vector
    position = reactive(...),
    ...
)
```

**`mod_export_server()`** returns:
```r
list(
    width = reactive(...),
    height = reactive(...),
    dpi = reactive(...),
    format = reactive(...)
)
```

### UI Helpers (`story_designer_ui.R`)

Functions that build accordion panels:
- `text_block_accordion()` - Title/subtitle/narrative/caption inputs
- `fine_tune_accordion()` - Alignment and line height controls
- `plot_settings_accordion()` - Theme and axis settings

### Code Generation (`code_generation.R`)

Functions that generate R code strings for the Code tab:

| Function | Generates |
|----------|-----------|
| `generate_theme_code()` | `styled_plot <- my_plot + theme_stwd() + theme(...)` |
| `generate_grid_code()` | Grid line theme modifications |
| `generate_palette_code()` | `scale_fill_manual(values = ...)` |
| `generate_block_code()` | `title_plot <- title_block(...)` |
| `generate_legend_code()` | `legend_plot <- legend_block(...)` |
| `generate_composition_code()` | Final patchwork composition |

**Important**: These generate strings, not code objects. All output should be parseable R code.

### State Management (`designer_defaults.R`)

**`default_input_values`** - Named list of all default values

**`reset_all_inputs()`** - Resets all Shiny inputs to defaults

**`build_theme_mods()`** - Builds ggplot2 theme object from input values

## Testing Approach

### Test Files

```
tests/testthat/
├── test-blocks.R          # Block functions and theme
├── test-code-generation.R # Code generation functions
├── test-colors.R          # highlight_colors, inline_legend, list_colors
├── test-layout.R          # story_layout, estimate_layout_heights
├── test-modules.R         # Module helper functions
```

### Testing Strategy

1. **Exported functions** - Test all public API
2. **Code generation** - Verify output is parseable R code
3. **Helper functions** - Test edge cases (NULL, empty, etc.)
4. **Skip Shiny reactives** - No shinytest2 (complex setup)

### Running Tests

```r
devtools::test()           # Run all tests
devtools::test_active_file() # Run current file
```

### Adding Tests

Use `describe()`/`it()` pattern:
```r
describe("my_function", {
    it("handles normal input", {
        result <- my_function("test")
        expect_equal(result, "expected")
    })

    it("handles NULL gracefully", {
        result <- my_function(NULL)
        expect_null(result)
    })
})
```

## Common Maintenance Tasks

### Adding a New Palette Package

1. Add to `Suggests` in DESCRIPTION
2. Update `get_palette_names()` in `palette_helpers.R`
3. Update `get_palette_colors()` in `palette_helpers.R`
4. Update `generate_palette_code()` in `code_generation.R`
5. Add tests

### Adding a New Block Type

1. Add function to `blocks.R`
2. Add `@export` tag
3. Use `create_text_block()` for consistency
4. Add tests to `test-blocks.R`
5. Run `devtools::document()`

### Modifying the Shiny App

1. UI changes: `story_designer.R` (UI section) or `story_designer_ui.R`
2. Server logic: `story_designer.R` (server section) or relevant module
3. Defaults: `designer_defaults.R`
4. Code output: `code_generation.R`

## Build & Check

```r
devtools::document()                    # Regenerate docs
devtools::check(remote = TRUE, manual = TRUE)  # Full CRAN check
devtools::test()                        # Run tests
devtools::install()                     # Install locally
```

## Release Checklist

1. Update version in DESCRIPTION
2. Run `devtools::check(remote = TRUE, manual = TRUE)`
3. Ensure 0 errors, 0 warnings
4. Run full test suite
5. Update NEWS.md if exists
6. Commit and push to Codeberg
7. Rebuild documentation site with qrtdown

## Dependencies

**Imports** (required):
- cli, ggplot2, grid, patchwork, rlang, stats, utils

**Suggests** (optional):
- marquee (required for block functions)
- shiny, bslib (required for story_designer)
- Palette packages: ggsci, MetBrewer, nord, PNWColors, rcartocolor, RColorBrewer, scico, viridis, wesanderson

## Code Style

- Use native pipe `|>` (not `%>%`)
- Use `cli` for user-facing messages
- Prefix Shiny functions with `shiny::` for CRAN compliance
- Use `@noRd` for internal functions (no Rd file generated)
