# pixel-smear

## description

Take the pixels bound by row/col min/max and replace them with the pixels found at the maximum row.  If `--smear-down` is enabled, then replace the region by pixels found at the minimum row.

## usage
Full list of options:

```bash
pixel-smear

Usage: pixel-smear --file ARG [--row-min ARG] [--row-max ARG] [--col-min ARG]
                   [--col-max ARG] [--smear-down]

Available options:
  -h,--help                Show this help text
  --file ARG               Image to manipulate
  --row-min ARG            Row to start pixel smearing
  --row-max ARG            Row to end pixel smearing
  --col-min ARG            Column to start pixel smearing
  --col-max ARG            Column to end pixel smearing
  --smear-down             Use the row min as the base row, instead of row max
```

## examples
![original](repo_assets/orig.jpg)

`pixel-smear --file orig.jpg --row-min 1500 --smear-down`

![smeared](repo_assets/orig-smeared.jpg)
