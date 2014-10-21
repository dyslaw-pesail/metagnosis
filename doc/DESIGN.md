DESIGN [draft]
==============

Voxel = record([attributes])
Block = { {C000, Voxel | Block },
          ...
          {C111, Voxel | Block },
          [attributes] }
Addr = [CXXX, CYYY,CZZZ, ... ]
Cluster = gen_server  (x, y, z) 
            set_voxel,
            get_voxel,
            save_after_timeout
