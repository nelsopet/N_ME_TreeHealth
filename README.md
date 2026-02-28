# N_ME_TreeHealth
MEIF SCI Funded study lead by UMFK on Picea and Spruce using reflectance spectroscopy to map leaf chemical, physical and physiologicalsigntures

## RGB quicklook slideshow (3s/frame)

The slideshow below is generated from all `raw_*_rd_rf_or` images in the matched N_ME flights.

![N_ME Spruce RGB Quicklooks (3s/frame)](data/rgb_quicklooks_nme_flights/rgb_quicklooks_slideshow_3s.gif)

Quicklook outputs and manifest:
- `data/rgb_quicklooks_nme_flights/*.png`
- `data/rgb_quicklooks_nme_flights/rgb_quicklook_manifest.tsv`

## Build quicklooks and slideshow

```bash
uv run python scripts/export_rgb_quicklooks.py \
  --pairs-table data/spruce_all_flight_images_rd_rf_or.tsv \
  --out-dir data/rgb_quicklooks_nme_flights

uv run python scripts/build_rgb_quicklook_slideshow.py \
  --input-dir data/rgb_quicklooks_nme_flights \
  --output-gif data/rgb_quicklooks_nme_flights/rgb_quicklooks_slideshow_3s.gif \
  --seconds-per-image 3
```
