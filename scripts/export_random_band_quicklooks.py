from __future__ import annotations

import argparse
import csv
import random
import re
from pathlib import Path

import numpy as np
import rasterio
from PIL import Image


DEFAULT_RGB_MANIFEST = Path("data/rgb_quicklooks_nme_flights/rgb_quicklook_manifest.tsv")
DEFAULT_OUT_ROOT = Path("data")
DEFAULT_MAX_DIM = 1600


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description=(
            "Export quicklook PNGs for the same source images used in RGB quicklooks, "
            "using random 3-band combinations between 400 and 800 nm."
        )
    )
    p.add_argument("--rgb-manifest", type=Path, default=DEFAULT_RGB_MANIFEST)
    p.add_argument("--out-root", type=Path, default=DEFAULT_OUT_ROOT)
    p.add_argument("--num-combos", type=int, default=5)
    p.add_argument("--seed", type=int, default=20260227)
    p.add_argument("--min-nm", type=float, default=400.0)
    p.add_argument("--max-nm", type=float, default=800.0)
    p.add_argument("--min-separation", type=float, default=30.0)
    p.add_argument("--max-dim", type=int, default=DEFAULT_MAX_DIM)
    p.add_argument("--p-low", type=float, default=2.0)
    p.add_argument("--p-high", type=float, default=98.0)
    return p.parse_args()


def _read_band_wavelengths_from_hdr(hdr_path: Path) -> list[float]:
    if not hdr_path.exists():
        return []
    text = hdr_path.read_text(encoding="utf-8", errors="ignore")
    vals: list[float] = []

    # Prefer explicit ENVI wavelength list when present.
    m_wave = re.search(r"wavelength\s*=\s*\{(.*?)\}", text, flags=re.IGNORECASE | re.DOTALL)
    if m_wave:
        for mm in re.finditer(r"([0-9]+(?:\.[0-9]+)?)", m_wave.group(1)):
            vals.append(float(mm.group(1)))
        if vals:
            return vals

    # Fallback for products that expose wavelength values in band names.
    m_names = re.search(r"band names\s*=\s*\{(.*?)\}", text, flags=re.IGNORECASE | re.DOTALL)
    if m_names:
        for tok in m_names.group(1).split(","):
            mm = re.search(r"([0-9]+(?:\.[0-9]+)?)", tok.strip())
            if mm:
                vals.append(float(mm.group(1)))
    return vals


def _nearest_band_index(wavelengths: list[float], target_nm: float) -> int:
    arr = np.asarray(wavelengths, dtype=np.float32)
    idx0 = int(np.argmin(np.abs(arr - target_nm)))
    return idx0 + 1


def _stretch_channel(ch: np.ndarray, p_low: float, p_high: float) -> np.ndarray:
    valid = np.isfinite(ch)
    if not np.any(valid):
        return np.zeros_like(ch, dtype=np.uint8)
    lo, hi = np.percentile(ch[valid], [p_low, p_high])
    if hi <= lo:
        return np.zeros_like(ch, dtype=np.uint8)
    out = (ch - lo) / (hi - lo)
    out = np.clip(out, 0.0, 1.0)
    return (out * 255.0).astype(np.uint8)


def _make_rgb_like_png(
    image_path: Path,
    out_png: Path,
    target_triplet_nm: tuple[float, float, float],
    max_dim: int,
    p_low: float,
    p_high: float,
) -> tuple[int, int, int, tuple[float, float, float]]:
    hdr_path = Path(f"{image_path}.hdr")
    wavelengths = _read_band_wavelengths_from_hdr(hdr_path)
    if not wavelengths:
        raise FileNotFoundError(f"Missing or unreadable HDR wavelengths: {hdr_path}")

    r_idx = _nearest_band_index(wavelengths, target_triplet_nm[0])
    g_idx = _nearest_band_index(wavelengths, target_triplet_nm[1])
    b_idx = _nearest_band_index(wavelengths, target_triplet_nm[2])

    picked_nm = (
        float(wavelengths[r_idx - 1]),
        float(wavelengths[g_idx - 1]),
        float(wavelengths[b_idx - 1]),
    )

    with rasterio.open(image_path) as src:
        h = src.height
        w = src.width
        scale = max(h / max_dim, w / max_dim, 1.0)
        out_h = max(1, int(h / scale))
        out_w = max(1, int(w / scale))

        r = src.read(r_idx, out_shape=(out_h, out_w), resampling=rasterio.enums.Resampling.average)
        g = src.read(g_idx, out_shape=(out_h, out_w), resampling=rasterio.enums.Resampling.average)
        b = src.read(b_idx, out_shape=(out_h, out_w), resampling=rasterio.enums.Resampling.average)

    rgb = np.stack(
        [
            _stretch_channel(r, p_low, p_high),
            _stretch_channel(g, p_low, p_high),
            _stretch_channel(b, p_low, p_high),
        ],
        axis=-1,
    )
    Image.fromarray(rgb, mode="RGB").save(out_png, optimize=True)
    return r_idx, g_idx, b_idx, picked_nm


def _is_valid_combo(vals_nm: tuple[float, float, float], min_separation: float) -> bool:
    a, b, c = vals_nm
    return (
        abs(a - b) >= min_separation
        and abs(a - c) >= min_separation
        and abs(b - c) >= min_separation
    )


def _choose_unique_triplets(
    wavelengths: list[float],
    num_combos: int,
    seed: int,
    min_nm: float,
    max_nm: float,
    min_separation: float,
) -> list[tuple[float, float, float]]:
    in_range = [w for w in wavelengths if min_nm <= w <= max_nm]
    if len(in_range) < 3:
        raise ValueError("Not enough wavelengths in the requested range to make 3-band triplets.")

    rng = random.Random(seed)
    combos: list[tuple[float, float, float]] = []
    combo_keys: set[tuple[int, int, int]] = set()
    attempts = 0
    max_attempts = 200000

    while len(combos) < num_combos and attempts < max_attempts:
        attempts += 1
        triplet = tuple(rng.sample(in_range, 3))
        if not _is_valid_combo(triplet, min_separation):
            continue
        key = tuple(int(round(v)) for v in triplet)
        if key in combo_keys:
            continue
        combo_keys.add(key)
        combos.append(triplet)

    if len(combos) < num_combos:
        raise RuntimeError(f"Unable to find {num_combos} unique valid band triplets in {attempts} attempts.")

    return combos


def main() -> None:
    args = parse_args()
    if not args.rgb_manifest.exists():
        raise FileNotFoundError(f"RGB manifest not found: {args.rgb_manifest}")
    args.out_root.mkdir(parents=True, exist_ok=True)

    with args.rgb_manifest.open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f, delimiter="\t"))

    valid_rows = [r for r in rows if Path((r.get("image_path") or "").strip()).exists()]
    if not valid_rows:
        raise RuntimeError("No existing source image paths found from RGB manifest.")

    first_image = Path((valid_rows[0].get("image_path") or "").strip())
    first_wavelengths = _read_band_wavelengths_from_hdr(Path(f"{first_image}.hdr"))
    if not first_wavelengths:
        raise RuntimeError(f"Could not read wavelengths from first image HDR: {first_image}.hdr")

    combos = _choose_unique_triplets(
        wavelengths=first_wavelengths,
        num_combos=args.num_combos,
        seed=args.seed,
        min_nm=args.min_nm,
        max_nm=args.max_nm,
        min_separation=args.min_separation,
    )

    set_manifest_rows: list[dict[str, str]] = []
    total_written = 0

    for combo in combos:
        combo_name = "_".join(str(int(round(v))) for v in combo)
        out_dir = args.out_root / f"{combo_name}_quicklooks_nme_flights"
        out_dir.mkdir(parents=True, exist_ok=True)

        per_set_rows: list[dict[str, str]] = []
        for row in valid_rows:
            image_path = Path((row.get("image_path") or "").strip())
            flight_id = (row.get("flight_id") or "").strip()
            stem = image_path.name
            out_name = f"{flight_id}_{stem}.png" if flight_id else f"{stem}.png"
            out_png = out_dir / out_name
            r_idx, g_idx, b_idx, picked_nm = _make_rgb_like_png(
                image_path=image_path,
                out_png=out_png,
                target_triplet_nm=combo,
                max_dim=args.max_dim,
                p_low=args.p_low,
                p_high=args.p_high,
            )
            total_written += 1
            per_set_rows.append(
                {
                    "image_path": str(image_path),
                    "quicklook_png": str(out_png.resolve()),
                    "flight_id": flight_id,
                    "target_r_nm": f"{combo[0]:.3f}",
                    "target_g_nm": f"{combo[1]:.3f}",
                    "target_b_nm": f"{combo[2]:.3f}",
                    "actual_r_nm": f"{picked_nm[0]:.3f}",
                    "actual_g_nm": f"{picked_nm[1]:.3f}",
                    "actual_b_nm": f"{picked_nm[2]:.3f}",
                    "r_band_index_1based": str(r_idx),
                    "g_band_index_1based": str(g_idx),
                    "b_band_index_1based": str(b_idx),
                }
            )

        per_set_manifest = out_dir / "quicklook_manifest.tsv"
        with per_set_manifest.open("w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(
                f,
                fieldnames=[
                    "image_path",
                    "quicklook_png",
                    "flight_id",
                    "target_r_nm",
                    "target_g_nm",
                    "target_b_nm",
                    "actual_r_nm",
                    "actual_g_nm",
                    "actual_b_nm",
                    "r_band_index_1based",
                    "g_band_index_1based",
                    "b_band_index_1based",
                ],
                delimiter="\t",
            )
            w.writeheader()
            w.writerows(per_set_rows)

        set_manifest_rows.append(
            {
                "set_folder": str(out_dir.resolve()),
                "set_name": combo_name,
                "rounded_r_nm": str(int(round(combo[0]))),
                "rounded_g_nm": str(int(round(combo[1]))),
                "rounded_b_nm": str(int(round(combo[2]))),
                "num_images": str(len(per_set_rows)),
                "manifest_tsv": str(per_set_manifest.resolve()),
            }
        )

    all_sets_manifest = args.out_root / "random_band_quicklook_sets.tsv"
    with all_sets_manifest.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(
            f,
            fieldnames=[
                "set_folder",
                "set_name",
                "rounded_r_nm",
                "rounded_g_nm",
                "rounded_b_nm",
                "num_images",
                "manifest_tsv",
            ],
            delimiter="\t",
        )
        w.writeheader()
        w.writerows(set_manifest_rows)

    print(f"Created {len(combos)} random band sets.")
    print(f"Wrote {total_written} quicklooks across all sets.")
    print(f"Set manifest: {all_sets_manifest.resolve()}")
    for row in set_manifest_rows:
        print(f"- {row['set_name']}: {row['num_images']} images")


if __name__ == "__main__":
    main()
