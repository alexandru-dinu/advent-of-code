xs = strsplit(fileread(argv(){1}), '\n');
e  = str2num(xs{1});
bs = strsplit(xs{2}, ',');

mask = cellfun(@(x) ~isequal(x, 'x'), bs);
bids = cellfun(@(x) str2num(x), bs(mask));

[tmin, imin] = min(arrayfun(@(b) b * ceil(e / b), bids));
printf("Part 1: %d\n", bids(imin) * (tmin - e))
