# knots_v_cutoff
This repo is a quick comparison of standardized indices for U.S. west coast groundfish species, comparing using the 'n_knots' argument in `sdmTMB::make_mesh()` to the 'cutoff' argument, representing cutoff distance. The 'n_knots' argument has been implemented in the NWFSC's `indexwc` package for index standardization.

To compare the cutoff distance approach to the 'n_knots' conventional approach, I looked at all models using the WCGBTS survey data, and for each found the cutoff distance that generated a mesh with the closest number of vertices (knots) to the mesh created using the 'n_knots' argument. For each, I implemented the same sdmTMB model, and for the models that passed all `sdmTMB::sanity()` checks, I overlayed the two indices (shown below). From these results, it appears that there is very little difference in indices generated from these two approaches. 

<img width="2700" height="2100" alt="image" src="https://github.com/user-attachments/assets/172bda16-0401-44d2-bd4d-039f436d849f" />


