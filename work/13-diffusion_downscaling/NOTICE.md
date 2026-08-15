# Notice

This repository is coursework for a university seminar. It is published for review and
reproduction of the reported results, not for redistribution or commercial use.

## Third-party code

`src/wm2024model.py` is **not original work**. It is derived from:

- **NVIDIA EDM** — Karras, T., Aittala, M., Aila, T., & Laine, S. (2022),
  *Elucidating the Design Space of Diffusion-Based Generative Models*.
  Source: [NVlabs/edm](https://github.com/NVlabs/edm).
  Licensed under
  [CC BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/).
  Copyright (c) 2022, NVIDIA CORPORATION & AFFILIATES. All rights reserved.

- **Modified by Robbie Watt (2024)** for climate downscaling, in
  [robbiewatt1/ClimateDiffuse](https://github.com/robbiewatt1/ClimateDiffuse) —
  see Watt, R., & Mansfield, L. (2024), *Generative diffusion-based downscaling for
  climate*.

The original copyright and licence headers are retained at the top of that file. Because
CC BY-NC-SA 4.0 carries a ShareAlike condition, any redistribution of this repository (or
of derivatives of that file) must remain non-commercial and carry the same terms.

`src/downscaling/sampling.py` reimplements the Heun/EDM sampler described in the same
Karras et al. (2022) paper.

## Everything else

All other code in `src/` — the dataset pipeline, training loop, inference, evaluation and
diagnostic scripts — was written for this project.
