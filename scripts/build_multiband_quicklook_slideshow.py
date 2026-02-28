from __future__ import annotations

import argparse
import csv
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


DEFAULT_RGB_DIR = Path("data/rgb_quicklooks_nme_flights")
DEFAULT_SETS_MANIFEST = Path("data/random_band_quicklook_sets.tsv")
DEFAULT_OUTPUT_GIF = DEFAULT_RGB_DIR / "rgb_quicklooks_slideshow_3s.gif"
DEFAULT_SECONDS_PER_STEP = 3.0
DEFAULT_FADE_SECONDS = 0.6
DEFAULT_FADE_FRAMES = 6
DEFAULT_MAX_WIDTH = 900
DEFAULT_MAX_COLORS = 128


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description=(
            "Build slideshow across RGB + random-band quicklooks per source image, "
            "with fade transitions every step."
        )
    )
    p.add_argument("--rgb-dir", type=Path, default=DEFAULT_RGB_DIR)
    p.add_argument("--sets-manifest", type=Path, default=DEFAULT_SETS_MANIFEST)
    p.add_argument("--output-gif", type=Path, default=DEFAULT_OUTPUT_GIF)
    p.add_argument("--seconds-per-step", type=float, default=DEFAULT_SECONDS_PER_STEP)
    p.add_argument("--fade-seconds", type=float, default=DEFAULT_FADE_SECONDS)
    p.add_argument("--fade-frames", type=int, default=DEFAULT_FADE_FRAMES)
    p.add_argument("--max-width", type=int, default=DEFAULT_MAX_WIDTH)
    p.add_argument("--max-colors", type=int, default=DEFAULT_MAX_COLORS)
    return p.parse_args()


def _resize_keep_aspect(img: Image.Image, max_width: int) -> Image.Image:
    if img.width <= max_width:
        return img
    ratio = max_width / float(img.width)
    new_h = max(1, int(img.height * ratio))
    return img.resize((max_width, new_h), Image.Resampling.LANCZOS)


def _pad_to_canvas(img: Image.Image, width: int, height: int) -> Image.Image:
    canvas = Image.new("RGB", (width, height), color=(0, 0, 0))
    x = (width - img.width) // 2
    y = (height - img.height) // 2
    canvas.paste(img, (x, y))
    return canvas


def _with_label(img: Image.Image, label: str) -> Image.Image:
    out = img.copy()
    draw = ImageDraw.Draw(out)
    font = ImageFont.load_default()
    pad = 8
    bbox = draw.textbbox((0, 0), label, font=font)
    tw = bbox[2] - bbox[0]
    th = bbox[3] - bbox[1]
    x = pad
    y = pad
    draw.rectangle((x - 4, y - 3, x + tw + 4, y + th + 3), fill=(0, 0, 0))
    draw.text((x, y), label, fill=(255, 255, 255), font=font)
    return out


def _load_set_dirs(sets_manifest: Path) -> list[Path]:
    if not sets_manifest.exists():
        raise FileNotFoundError(f"Sets manifest not found: {sets_manifest}")
    with sets_manifest.open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f, delimiter="\t"))
    set_dirs: list[Path] = []
    for row in rows:
        set_folder = Path((row.get("set_folder") or "").strip())
        if not set_folder.exists():
            raise FileNotFoundError(f"Set folder listed in manifest not found: {set_folder}")
        set_dirs.append(set_folder)
    if not set_dirs:
        raise ValueError(f"No set folders found in manifest: {sets_manifest}")
    return set_dirs


def main() -> None:
    args = parse_args()
    if not args.rgb_dir.exists():
        raise FileNotFoundError(f"RGB directory not found: {args.rgb_dir}")
    if args.fade_frames < 1:
        raise ValueError("--fade-frames must be >= 1")
    if args.seconds_per_step <= 0:
        raise ValueError("--seconds-per-step must be > 0")
    if args.fade_seconds < 0:
        raise ValueError("--fade-seconds must be >= 0")

    set_dirs = _load_set_dirs(args.sets_manifest)
    rgb_pngs = sorted(p for p in args.rgb_dir.glob("*.png") if p.is_file() and not p.name.startswith("."))
    if not rgb_pngs:
        raise ValueError(f"No RGB PNGs found in {args.rgb_dir}")

    # Build ordered stills per source image: RGB first, then each random-band set.
    ordered_items: list[tuple[Path, str]] = []
    for rgb_path in rgb_pngs:
        ordered_items.append((rgb_path, "RGB"))
        for set_dir in set_dirs:
            candidate = set_dir / rgb_path.name
            if not candidate.exists():
                raise FileNotFoundError(f"Missing matching quicklook: {candidate}")
            ordered_items.append((candidate, set_dir.name.replace("_quicklooks_nme_flights", "")))

    # Preload and normalize all still images to one canvas size.
    resized: list[Image.Image] = []
    labels: list[str] = []
    for p, label in ordered_items:
        with Image.open(p) as im:
            im = im.convert("RGB")
            im = _resize_keep_aspect(im, args.max_width)
            resized.append(im.copy())
            labels.append(label)
    canvas_width = max(im.width for im in resized)
    canvas_height = max(im.height for im in resized)
    stills = [_with_label(_pad_to_canvas(im, canvas_width, canvas_height), lab) for im, lab in zip(resized, labels)]

    step_ms = int(round(args.seconds_per_step * 1000.0))
    fade_ms_total = int(round(args.fade_seconds * 1000.0))
    hold_ms = max(0, step_ms - fade_ms_total)
    fade_frame_ms = max(1, int(round(fade_ms_total / args.fade_frames))) if fade_ms_total > 0 else 0

    frames: list[Image.Image] = []
    durations: list[int] = []

    for i in range(len(stills) - 1):
        curr = stills[i]
        nxt = stills[i + 1]

        # Hold current still before transition.
        if hold_ms > 0:
            frames.append(curr.copy())
            durations.append(hold_ms)

        # Fade current -> next within the remaining step duration.
        if fade_ms_total > 0:
            for j in range(1, args.fade_frames + 1):
                alpha = j / float(args.fade_frames)
                frames.append(Image.blend(curr, nxt, alpha))
                durations.append(fade_frame_ms)
        elif hold_ms == 0:
            # Degenerate case: no hold and no fade; keep one frame to preserve timeline.
            frames.append(curr.copy())
            durations.append(step_ms)

    # Final still.
    frames.append(stills[-1].copy())
    durations.append(step_ms)

    args.output_gif.parent.mkdir(parents=True, exist_ok=True)

    # Quantize to a shared palette to reduce GIF size significantly.
    if args.max_colors < 2 or args.max_colors > 256:
        raise ValueError("--max-colors must be in [2, 256]")
    palette_base = frames[0].convert("P", palette=Image.Palette.ADAPTIVE, colors=args.max_colors)
    paletted_frames = [palette_base]
    for fr in frames[1:]:
        paletted_frames.append(fr.quantize(palette=palette_base))

    paletted_frames[0].save(
        args.output_gif,
        save_all=True,
        append_images=paletted_frames[1:],
        duration=durations,
        loop=0,
        disposal=2,
        optimize=True,
    )

    print(f"Source images: {len(rgb_pngs)}")
    print(f"Band variants per source image (including RGB): {1 + len(set_dirs)}")
    print(f"Ordered stills: {len(stills)}")
    print(f"GIF frames: {len(frames)}")
    print(f"Step duration (ms): {step_ms}")
    print(f"Hold/Fade (ms): {hold_ms}/{fade_ms_total}")
    print(f"Output GIF: {args.output_gif.resolve()}")


if __name__ == "__main__":
    main()
