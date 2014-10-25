-include("block_attr.hrl").
-record(block, {attr = #block_attr{},
                c000 = empty,
                c001 = empty,
                c010 = empty,
                c011 = empty,
                c100 = empty,
                c101 = empty,
                c110 = empty,
                c111 = empty}).
