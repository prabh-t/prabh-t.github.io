Version History

Version 4.1
- Further strictness by defining strict types for pos, vel and mass
- used unpacked

Version 4
- Read bodies from file.
- Made use of segments to update velocities in parallel.

Version 3
- Made use of arrays (for pos, vel and mass) instead of lists which gave better performance.
- Updated velocity using parListChunk strategy.

Version 2
- Made use of Array.accum to update initial velocities with accumulated velocities changes.
- Added more parallel evaluation but main function still not parallelised.

Version 1.1
- Made all functions tail recursive and made use of accumulating params to fix stack overflow issue with increasing number of bodies.
- Introduced strictness in some functions/values evaluation. Added main function, gave proper names to functions and chained function calls.
- Introduced parallel evaluation in some functions using strategies.

Version 1
- Adapted Java implementation of nbody simulation in Haskell.
- Defined new type 'Body' as 3 doubles for positions and velocities and 1 double for mass.
- Made use of 5 bodies with positions, velocities and masses hardcoded in source file.
- Results compared with and matched Java implementation.
- No compilation/main function. Used GHCi.
- No performance consideration at all.
- Sequential. 