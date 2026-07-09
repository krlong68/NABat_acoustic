# out_csv

This directory will contain CSV files that are created by `reformat.R`. These
files will not be manually produced, but subdirectories should be organized as
follows so that the script places them correctly.

```
out_csv/
├── Survey_Year/
│   └── Grid_ID/
│       └── output.CSV
└── README.md
```

The above subdirectory names will be replaced to suit your use case. Example
below: the survey was performed in 2025 and the grid ID was 113851.

```
out_csv/
├── 2025/
│   └── 113851/
│       └── 113851_Mobile_20250828.csv
└── README.md
```

**Note: in the example above, `113851_Mobile_20250828.csv` is created by running
`reformat.R`, not placed there by the user. The user just needs to set up the
`2025/` and `113851/` subdirectories.